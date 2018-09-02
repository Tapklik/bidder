-module(bidder_model).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").
-include_lib("../lib/amqp_client/include/amqp_client.hrl").

-export([start_link/0]).

-export([get_prediction_async/1, test_async/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	cmp,
	tid,
	timestamp,
	hash,
	pacing_timestamp
}).

-define(INTERVAL, 4 * 1000).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%%
%%
start_link() ->
	gen_server:start_link(?MODULE, [], []).


get_prediction_async(ModelBR) ->
	bidder_model_pool:take_async({ModelBR, self()}).


test_async() ->
	ModelBR = get_sample_model_br(),
	bidder_model_pool:take_async({ModelBR, self()}),
	receive
		{no_bid, Error} ->
			{no_bid, Error};
		{bid, Resp} ->
			{bid, Resp}
	after ?MODEL_TIMEOUT ->
		{no_bid, model_timeout}
	end.


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @private
%%
%%
init([]) ->
	process_flag(trap_exit, true),
	?INFO("BIDDER_MODEL: Started on initiated on node ~p", [node()]),
	erlang:send_after(?INTERVAL, self(), {init}),
	{ok, #state{}}.



handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


%%
%%
%%
handle_info({init}, State) ->
	case ?ENV(model_enabled) of
		true ->
			bidder_model_pool:start_pool([]);
		ok ->
			ok
	end,
	{noreply, State};

handle_info({stop}, State) ->
	{stop, shutdown, State};

handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

%%
%% @private
%% Stops the process gracefully, and cleans up all the ETS tables
%%
terminate(shutdown, _State) ->
	?ERROR("BIDDER_MODEL: Stopped on node ~p", [node()]),
	ok;
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

get_sample_model_br() ->
	#{
		<<"bid-id">> => <<"15304-12b4-4282-a52b-370601e07848">>,
		<<"device_make">> => <<"Apple">>,
		<<"device_model">> => <<"">>,
		<<"device_os">> => <<"Windows">>,
		<<"device_type">> => 2,
		<<"device_ua">> => <<"Chrome">>,
		<<"city">> => <<"Dubayy">>,
		<<"country">> => <<"ARE">>,
		<<"hour_of_week">> => 153,
		<<"position">> => 0,
		<<"content_rating">> => <<"DV-G">>,
		<<"content_language">> => <<"en">>,
		<<"amp">> => 0,
		<<"mobile">> => 0,
		<<"page">> => <<"https://www.stanza.co/@arsenal">>,
		<<"publisher_type">> => <<"site">>,
		<<"publisher_country">> => <<"US">>,
		<<"adomain">> => <<"beefbar.com">>,
		<<"class">> => <<"banner">>,
		<<"cmp">> => <<"79174d3d24">>,
		<<"crid">> => <<"9c554ce8-739e-11e8-a8aa-0265f72072a0">>,
		<<"click">> => 0,
		<<"dimension">> => <<"300x250">>,
		<<"data-segment">> => <<"[(695 => 0.3), (1358 => 1), (1274 => 0.1), (1142 => 0.1), (1760 => 0.1)]">>,
		<<"viewability">> => 0,
		<<"session_depth">> => 300,
		<<"categories">> => <<"[IAB15,IAB15-4,IAB19,IAB19-30,IAB22]">>,
		<<"click_through_rate">> => 0,
		<<"video_completion_rate">> => 0.002
	}.