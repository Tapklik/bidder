-module(bidder_cache).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").


-export([start_link/0, put/3, get/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-record(state, {
	ts
}).

-define(INTERVAL, 5000).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link(?MODULE, [], []).


put(BidId, Ts, BRData) ->
	ets:insert(bids_cache, {BidId, Ts, BRData}).


get(BidId) ->
	case ets:lookup(bids_cache, BidId) of
		[{_, _, BRData} | _] ->
			{ok, BRData};
		[] ->
			{error, not_found}
	end.


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	ets:new(bids_cache, [public, named_table, {write_concurrency, true}]),
	%% Init
	erlang:send_after(?INTERVAL, self(), {interval}),
	{ok, #state{}}.


handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({interval}, State) ->
	Ts1 = time_server:get_timestamp(),
	case State#state.ts of
		undefined -> ok;
		Ts0 when Ts0 == Ts1 -> ok;
		Ts0 ->
			TsToDelete = Ts0 - 600,
			spawn(
				fun() ->
					ets:match_delete(bids_cache, {'$1', TsToDelete, '_'})
				end
			)
	end,
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{ts = Ts1}};
handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
