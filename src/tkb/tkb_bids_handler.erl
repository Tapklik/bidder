-module(tkb_bids_handler).

-behaviour(gen_server).

-include("bidder_global.hrl").
-include("lager.hrl").


-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-record(state, {
	auct_pid,
	pool_name,
	debug,
	bid_id,
	from,
	t1
}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link(?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ok ,State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({{From, br, BidId, BR, TimeStamp, DebugBid}, Poolname}, State) ->
	%% STAT
	T1 = erlang:monotonic_time(),

	{ok, AuctionPid} = tkb_auction_process:start_link(self(), BidId, BR, TimeStamp, DebugBid),
	L = ets:match_object(cmp_list, '_'),
	NumCmp = length(L),
	NumPassCmp = ?ENV(campaigns_pass_num, 20),
	SelectRatio = case NumCmp of
					  0 -> 0;
				      X when NumPassCmp / X >= 1.0 -> 1.0;
					  X -> NumPassCmp / X
				  end,
	spawn_bidders(L, BR, BidId, AuctionPid, SelectRatio, TimeStamp, DebugBid),
	{noreply, State#state{
		from = From,
		pool_name = Poolname,
		bid_id = BidId,
		t1 = T1,
		debug = DebugBid
	}};
handle_info({auction_rsp, BidId, RSPmap}, State) when BidId == State#state.bid_id->
	From = State#state.from,
	Poolname = State#state.pool_name,
	DebugBid = State#state.debug,
	From ! {self(), rsp, BidId, RSPmap},
	pooler:return_member(Poolname, self()),

	%% STAT: Calculate bid response time
	T2 = erlang:monotonic_time(),
	Time1 = erlang:convert_time_unit(T2 - State#state.t1, native, milli_seconds),
	statsderl:timing("rsp.time_bidder", Time1, ?STATS_P),
	{noreply, State};
handle_info(stop, State) ->
	{stop, normal, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

spawn_bidders(L, BR, BidId, AuctionPid, SelectRatio, TimeStamp, DebugBid) ->
	lists:foreach(
		fun({Cmp, AccId, _Pid, CmpTid, Rate})->
			case (Rate * SelectRatio) > rand:uniform() of
				true ->
					proc_lib:spawn(tkb_bidder_process, process_bid, [AccId, Cmp, BR, BidId, CmpTid, AuctionPid, TimeStamp, DebugBid]);
				false ->
					ok
			end
		end
		,L).