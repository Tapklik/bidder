-module(tkb_bidder_process).

%-include("bidder_global.hrl").

-export([process_bid/8]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

process_bid(AccId, Cmp, BR, BidId, CmpTid, AuctionPid, TimeStamp, DebugBid) ->
	RSPmap = #{
		<<"price">> => rand:uniform(100)/10,
		<<"weight">> => 1,
		<<"cid">> => Cmp,
		<<"acc">> => AccId,
		<<"id">> => tk_maps:get([<<"id">>], BR)
	},
	%% Updating CMP counters -----------------
	ets:update_counter(CmpTid, <<"bid_requests">>, 1),
	%% ---------------------------------------
	erlang:send_after(40, self(), {done, AuctionPid, BR, Cmp, BidId, CmpTid, RSPmap}),
	case tkb_bids_filter:filter_bid(CmpTid, BR) of
		{fail, Reason} ->
			ReasonBin1 = reason_to_binary(Reason),
			ReasonBin2 = <<"fail - ", ReasonBin1/binary>>,
			bidder_data:save_bidder_bid(TimeStamp, BidId, Cmp, ReasonBin2, 0.0),
			log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, ReasonBin2}], DebugBid);
		{pass, Cr} ->
			receive
				{done, AuctionPid, BR, Cmp, BidId, CmpTid, RSPmap} ->
					%% Updating CMP counters -----------------
					ets:update_counter(CmpTid, <<"bids">>, 1),
					%% ---------------------------------------
					RSPmap2 = RSPmap#{<<"creative">> => Cr},
					Crid = tk_maps:get([<<"crid">>], Cr),
					AccId = tk_maps:get([<<"acc">>], RSPmap),
					BidPrice = tk_maps:get([<<"price">>], RSPmap),
					bidder_data:save_bidder_bid(TimeStamp, BidId, Cmp, Crid, BidPrice),
					log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, RSPmap2}], DebugBid),
					AuctionPid ! {bid, Cmp, BidId, RSPmap2}
			end
	end.


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

log_bid(_, _, false) ->
	ok;
log_bid(BidId, List, true) when is_list(List)->
	Bid1 = #{<<"id">> => BidId},
	Bid2 = lists:foldl(
		fun({K, V}, Acc)->
			Acc#{K => V}
		end
		, Bid1, List),
	rmq:publish(bids_debug, term_to_binary(Bid2)).

reason_to_binary(Reason) when is_binary(Reason) ->
	Reason;
reason_to_binary(Reason) when is_atom(Reason) ->
	atom_to_binary(Reason, latin1);
reason_to_binary(Reason) when is_tuple(Reason) ->
	{R1, R2} = Reason,
	B1 = reason_to_binary(R1),
	B2 = reason_to_binary(R2),
	<<B1/binary, "_", B2/binary>>;
reason_to_binary(Reason) when is_list(Reason) ->
	reason_to_binary(hd(Reason)).