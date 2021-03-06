-module(tkb_auction_process).

-include("global.hrl").
-include("lager.hrl").


-export([start_link/5, init/1]).

-export([
	system_code_change/4, system_continue/3,
	system_terminate/4, write_debug/3
]).

-record(state, {
	current_bid,
	count,
	bid_id,
	br,
	bids,
	timestamp,
	debug
}).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%
start_link(Parent, BidId, BR, TimeStamp, DebugBid) ->
	proc_lib:start_link(?MODULE, init, [[Parent, BidId, BR, TimeStamp, DebugBid]]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([Parent, BidId, BR, TimeStamp, DebugBid]) ->
	Debug = sys:debug_options([]),
	proc_lib:init_ack(Parent, {ok, self()}),
	State = #state{
		current_bid = invalid_rsp,
		count = 0,
		bid_id = BidId,
		br = BR,
		bids = [],
		timestamp = TimeStamp,
		debug = DebugBid
	},
	erlang:send_after(?AUCTION_TIMEOUT, self(), {auction_timeout}),
	loop(Parent, Debug, State).

loop(Parent, Debug, State) ->
	receive
		{system, From, Request} ->
			sys:handle_system_msg(
				Request, From, Parent, ?MODULE, Debug, State
			);
		{bid, Cmp, BidId, Bid} ->
			CurrentCount = State#state.count,
			CurrentBid = State#state.current_bid,
			CurrentPrice = case CurrentBid of
							   invalid_rsp ->
								   0;
							   _ ->
								   tk_maps:get([<<"price">>], CurrentBid)
						   end,
			NewPrice = tk_maps:get([<<"price">>], Bid),
			Bids2 = [#{<<"cmp">> => Cmp, <<"price">> => NewPrice} | State#state.bids],
			case NewPrice > CurrentPrice of
				true ->
					NewState = State#state{
						current_bid = Bid,
						count = CurrentCount + 1,
						bids = Bids2
					};
				_ ->
					NewState = State#state{
						current_bid = CurrentBid,
						count = CurrentCount + 1,
						bids = Bids2
					}
			end,
			loop(Parent, Debug, NewState);
		{auction_timeout} ->
			RSP = State#state.current_bid,
			BR = State#state.br,
			BidId = State#state.bid_id,
			TimeStamp = State#state.timestamp,
			case RSP of
				invalid_rsp ->
					ok;
				R ->
					%% Save bid in temp table waiting for win notification
					bidder_wins:save_bid(TimeStamp, BidId, BR, R)
			end,
			Parent ! {auction_rsp, BidId, RSP}
	end.

write_debug(Dev, Event, Name) ->
	io:format(Dev, "~p event = ~p~n", [Name, Event]).

system_continue(Parent, Debug, State) ->
	loop(Parent, Debug, State).

system_terminate(Reason, _Parent, _Debug, _State) ->
	exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%



