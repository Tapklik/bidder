-module(tkb_bidder_process).

-export([process_bid/9]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

process_bid(AccId, Cmp, BR, BidId, ImpId, CmpTid, AuctionPid, TimeStamp, DebugBid) ->

	bidder_stats:increment(bid_request, CmpTid), %% Cmp stat update
	case tkb_bids_filter:filter_bid(CmpTid, BR) of

		{fail, Reason} ->
			{ReasonAtom, ReasonBin1} = check_reason(Reason),
			ReasonBin2 = <<"fail - ", ReasonBin1/binary>>,
			bidder_stats:increment(ReasonAtom, CmpTid), %% Cmp stat update
			bidder_data:save_bidder_bid(TimeStamp, BidId, Cmp, ReasonBin2, 0.0),
			log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, ReasonBin2}], DebugBid);

		{pass, Cr} ->
			{_, Config} = bidder_cmp:dirty_read_cmp(CmpTid, <<"config">>),
			Adomain = tk_maps:get([<<"adomain">>], Config, <<"">>),
			Bid = tk_maps:get([<<"bid">>, <<"bid">>], Config, 0),
			BidType = tk_maps:get([<<"bid">>, <<"type">>], Config, <<"random">>),
			BidFloor = tk_maps:get([<<"bidfloor">>], Cr, 0.0),
			Crid = tk_maps:get([<<"crid">>], Cr),
			Test = tk_maps:get([<<"test">>], BR),
			case tkb_bidder_bid:calc_bid(BidType, Bid, BidFloor) of
				no_bid ->
					bidder_data:save_bidder_bid(TimeStamp, BidId, Cmp, <<"fail - bidfloor">>, 0.0),
					bidder_stats:increment(failed_bidfloor, CmpTid), %% Cmp stat update
					log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, <<"fail - bidfloor">>}], DebugBid);
				BidPrice ->
					RSPmap = #{
						<<"price">> => BidPrice,
						<<"weight">> => 1,
						<<"cid">> => Cmp,
						<<"acc">> => AccId,
						<<"id">> => tk_maps:get([<<"id">>], BR),
						<<"test">> => Test,
						<<"creative">> => Cr#{
							<<"impid">> => ImpId,
							<<"adomain">> => Adomain
						}
					},
					%% 0 means "Live mode", 1 is "Test mode"
					case Test of
						1 -> ok;
						0 ->
							bidder_data:save_bidder_bid(TimeStamp, BidId, Cmp, Crid, BidPrice)
					end,
					bidder_stats:increment(bid, CmpTid), %% Cmp stat update
					log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, RSPmap}], DebugBid),
					AuctionPid ! {bid, Cmp, BidId, RSPmap}
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
log_bid(BidId, List, true) when is_list(List) ->
	Bid1 = #{<<"id">> => BidId},
	Bid2 = lists:foldl(
		fun({K, V}, Acc) ->
			Acc#{K => V}
		end
		, Bid1, List),
	rmq:publish(bids_debug, term_to_binary(Bid2)).


check_reason({[<<"cat">>], include}) ->
	{failed_cat, <<"cat">>};
check_reason({[<<"cat">>], exclude}) ->
	{failed_cat, <<"cat">>};
check_reason({[<<"budget">>, <<"hourofweek">>], include}) ->
	{failed_hourofweek, <<"hourofweek">>};
check_reason(pacing_rate) ->
	{failed_budget, <<"budget">>};
check_reason({[<<"user">>, <<"gender">>], include}) ->
	{failed_user, <<"user">>};
check_reason({[<<"device">>,<<"type">>], include}) ->
	{failed_device, <<"device">>};
check_reason({[<<"country">>], include}) ->
	{failed_geo, <<"geo">>};
check_reason(no_matching_creative) ->
	{failed_creative, <<"creative">>};
check_reason(Other) ->
	{failed_other, reason_to_binary(Other)}.


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