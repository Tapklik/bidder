-module(bidder_model_pool).

-behaviour(pool).

-include("global.hrl").
-include("lager.hrl").

-export([start_pool/1, take/1, take_async/1]).

-export([init/1, handle_take/3, handle_take_async/2,
	handle_return/1, handle_info/2, terminate/2]).

-record(state, {
	socket,
	stream_ref,
	status = free,
	from
}).

-define(POOL, #{
	mod => bidder_model_pool,
	num_workers => 500,
	dispatch_ets => pool0_dets,
	dispatch_type => hash,
	worker_ets => pool1_wets
}).
-define(CONNECTION_RETRY, 20000). % retry connection after 3 mins

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec start_pool(term()) -> {ok, started}.
start_pool(Args) ->
	pool:start_pool(?POOL, Args).


-spec take(term()) -> {reply, term(), term()} | no_workers_avaliable.
take(Msg) ->
	pool:take_worker(?POOL, Msg).


-spec take_async(term()) -> ok | no_workers_avaliable.
take_async({ModelBR, From}) ->
	pool:take_worker_async(?POOL, {ModelBR, From}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
	gun:open("localhost", ?ENV(model_port)),
	{ok, #state{}}.


handle_take(_Msg, _From, State) ->
	{reply, ok, State}.


handle_take_async({ModelBR, From}, State) when State#state.status == free ->
	Socket = case State#state.socket of
				 undefined ->
					 {ok, S} = gun:open("localhost", ?ENV(model_port)),
					 S;
				 S -> S
			 end,
	Body = jsx:encode(ModelBR),
	StreamRef = gun:post(Socket, "/transform", [
		{<<"content-type">>, "application/json"}
	], Body),
	{noreply, State#state{stream_ref = StreamRef, from = From}};
handle_take_async({_, From}, State) ->
	?WARN("BIDDER_MODEL: Connection (~p) busy or restarting!", [State#state.socket]),
	From ! {no_bid, ctr_prediction},
	pool:return_worker(self()),
	{noreply, State}.


handle_return(State) ->
	{noreply, State}.


handle_info({gun_response, _, _, _, _, _}, State) ->
	{noreply, State};
handle_info({gun_data, _, StreamRef, _, Data}, State = #state{from = From}) when StreamRef == State#state.stream_ref
	andalso State#state.from =/= undefined ->
	try
		DataDecoded = jsx:decode(Data, [return_maps]),
		From ! {bid, DataDecoded}
	catch
		_:_ ->
			From ! {no_bid, ctr_prediction}
	end,
	pool:return_worker(self()),
	{noreply, State#state{status = free}};
handle_info({gun_up, Socket, _}, State) ->
	{noreply, State#state{socket = Socket, status = free}};
handle_info({gun_down, _, _, _, _, _}, State) ->
	{noreply, State#state{status = free, socket = undefined}};
handle_info({'DOWN', _, _, _, _}, State) ->
	{stop, shutdown, State};
handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.


terminate(_Reason, _State) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
