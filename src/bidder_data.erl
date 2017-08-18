-module(bidder_data).

-behaviour(gen_server).

-include("bidder_global.hrl").
-include("lager.hrl").
-include_lib("../lib/amqp_client/include/amqp_client.hrl").

-export([
	start_link/0,
	get_current_tid/0,
	save_bid/4,
	save_bidder_bid/5,
	mark_win/1, mark_click/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-record(state, {
	server,
	ts = 0,
	ts_tid,
	ts_dict = [],
	db
}).

-define(INTERVAL, 2000).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

save_bid(TimeStamp, BidId, BR, RSP) ->
	SaveBid = ?BIDS_SAVE_PCTG >= rand:uniform(),
	BRjson = jsx:encode(BR),
	{AccId, Cmp, Crid, BidPrice} = check_rsp_output(RSP),
	%{ok, {TimeStamp, Tid}} = get_current_tid(),
	Data = #{
		<<"timestamp">> => TimeStamp,	% time stamp (5 mins)
		<<"bid_id">> => BidId,          % id
		<<"acc">> => AccId,            	% account id
		<<"cmp">> => Cmp,            	% campaign id
		<<"br">> => BR,         		% br
		<<"rsp">> => RSP,           	% creative id
		<<"include">> => SaveBid		% include
	},
	cache:put(bids_cache, BidId, Data).

save_bidder_bid(TimeStamp, BidId, Cmp, Crid, BidPrice) ->
	ok.

% TODO check if we still need to send every bidder bid to datawarehouse
save_bidder_bid2(TimeStamp, BidId, Cmp, Crid, BidPrice) ->
	Data = #{
		<<"timestamp">> => TimeStamp,	% time stamp (5 mins)
		<<"bid_id">> => BidId,          % id
		<<"cmp">> => Cmp,            	% campaign id
		<<"crid">> => Crid,         	% creative id
		<<"bid_price">> => BidPrice     % bid price
	},
	Value = jsx:encode(Data),
	Client = c1,
	Topic  = <<"bidders">>,
	PartitionFun = fun(_Topic, PartitionsCount, _Key, _Value) ->
		{ok, crypto:rand_uniform(0, PartitionsCount)}
				   end,
	brod:produce(Client, Topic, PartitionFun, <<"">>, Value).

mark_win(WinBin) ->
	WinMap = binary_to_term(WinBin),
	#{
		<<"timestamp">> := _TimeStamp,    	% time stamp (5 mins)
		<<"bid_id">> := BidId,          	% id
		<<"cmp">> := _Cmp,                	% campaign id
		<<"crid">> := Crid,                	% creative id
		<<"win_price">> := WinPrice    	 	% win price
	} = WinMap,
	Bid = cache:get(bids_cache, BidId),
	Data = Bid#{
		<<"crid">> => Crid,
		<<"win_price">> => trunc(WinPrice),
		<<"imps">> => 1,
		<<"clicks">> => 0
 	},
	Data2 = jsx:encode(Data),
	cache:put(wins_cache, BidId, Data),
	Topic  = <<"wins">>,
	publish_to_kafka(Topic, Data2).

mark_click(ClickBin) ->
	ClickMap = binary_to_term(ClickBin),
	#{
		<<"timestamp">> := _TimeStamp,    	% time stamp (5 mins)
		<<"bid_id">> := BidId,          	% id
		<<"cmp">> := _Cmp,                	% campaign id
		<<"crid">> := Crid              	% creative id
	} = ClickMap,
	case cache:get(wins_cache, BidId) of
		undefined ->
			ok;
		Bid ->
			Clicks = tk_maps:get([<<"clicks">>], Bid),
			Data = Bid#{
				<<"imps">> => 0,
				<<"win_price">> => 0,
				<<"crid">> => Crid,
				<<"clicks">> => Clicks + 1
			},
			Data2 = jsx:encode(Data),
			Topic  = <<"wins">>,
			publish_to_kafka(Topic, Data2)
	end.

publish_to_kafka(Topic, Load) ->
	Client = c1,
	case ?ENV(kafka_enabled) of
		true ->
			PartitionFun = fun(_Topic, PartitionsCount, _Key, _Value) ->
				{ok, crypto:rand_uniform(0, PartitionsCount)}
						   end,
			%% TODO Watch for spawning a new function here only for Kafka pub
			spawn(fun()-> brod:produce(Client, Topic, PartitionFun, <<"">>, Load) end);
		_ ->
			ok
	end.


get_current_tid() ->
	gen_server:call(?MODULE, {get_current_tid}).

get_all_tids() ->
	gen_server:call(?MODULE, {get_all_tids}).

%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @private
%% Starts the gen_server loop by sending an {interval} msg to initiate.
%%
%%
init([]) ->
	process_flag(trap_exit, true),
	erlang:send_after(?INTERVAL, self(), {interval}),
	{ok, #state{}}.

handle_call({get_current_tid}, _From, State) ->
	{reply, {ok, State#state.ts_tid}, State};
handle_call({get_all_tids}, _From, State) ->
	{reply, {ok, State#state.ts_dict}, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

%%
%% @private
%%
handle_info({interval}, State) ->
	Ts0 = State#state.ts,
	Ts = get_current_ts(),
	case Ts == Ts0 of
		true ->
			NewState = State;
		false ->
			TsDict0 = State#state.ts_dict,
			%% create table for current ts
			{Tid, TsDict1} = create_table(Ts, TsDict0),
			%% Delete data tables which are older than WIN_NOTIFICATION_WAIT_TS
			WinWait = ?ENV(win_notification_wait_ts, 3),
			TsDelete = subtract_ts(Ts, WinWait + 1),
			TsDict2 = delete_table(TsDelete, TsDict1),
			%% store previous ts table
			spawn_opt(fun() -> store_data(subtract_ts(Ts, 3), TsDict0) end, [{priority, low}]),
			NewState = State#state{
				ts = Ts,
				ts_tid = {Ts, Tid},
				ts_dict = TsDict2
			}

	end,
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, NewState};
handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

%%
%% @private
%% Error in case of exit
%%
terminate(_Reason, _State) ->
	?ERROR("BIDDER: Bids data module has stopped on node ~p", [node()]).

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

create_table(Ts, TsDict) ->
	Tid = ets:new(data_bids_table, [public, set, {write_concurrency, true}]),
	{Tid, [{Ts, Tid} | TsDict]}.

delete_table(Ts, TsDict) ->
	case proplists:get_value(Ts, TsDict, not_found) of
		not_found ->
			TsDict;
		Tid ->
			ets:delete(Tid),
			proplists:delete(Ts, TsDict)
	end.

store_data(Ts, TsDict) ->
	case proplists:get_value(Ts, TsDict, not_found) of
		not_found ->
			ok;
		Tid ->
			Data = ets:match(Tid, {'_', true, '$1'}),
			store_data_internal(Data)
	end.

store_data_internal(Data) ->
	Data2 = [H || [H | _] <- Data],
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	case riakc_ts:put(Pid, "bids2", Data2) of
		ok ->
			?INFO("BIDDER: Bids data has been successfully saved to database.", []);
		E ->
			?INFO("BIDDER: There was an error saving bids data database. (Error: ~p)", [E])
	end.

check_rsp_output(RSP) ->
	AccId = tk_maps:get([<<"acc">>], RSP),
	Cmp = tk_maps:get([<<"cid">>], RSP),
	Crid = tk_maps:get([<<"creative">>, <<"crid">>], RSP),
	BidPrice = tk_maps:get([<<"price">>], RSP),
	{AccId, Cmp, Crid, BidPrice}.

get_current_ts() ->
	TsLength = ?ENV(ts_length, 300),
	{A, B, _C} = erlang:timestamp(),
	T = A * 1000000 + B,
	trunc(T / TsLength) * TsLength * 1000.

subtract_ts(Ts, Periods) ->
	TsLength = ?ENV(ts_length, 300),
	Ts - (Periods * TsLength * 1000).

%% @hidden
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[Val | _] -> Val;
		[] -> Default
	end.
