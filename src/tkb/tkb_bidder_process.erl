-module(tkb_bidder_process).

-export([process_bid/9]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

process_bid(AccId, Cmp, BR, BidId, ImpId, CmpTid, AuctionPid, Timestamp, DebugBid) ->

	bidder_stats:increment(bid_request, CmpTid), %% Cmp stat update
	case tkb_bids_filter:filter_bid(CmpTid, BR) of

		{fail, Reason} ->
			{ReasonAtom, ReasonBin1} = check_reason(Reason),
			ReasonBin2 = <<"fail - ", ReasonBin1/binary>>,
			bidder_stats:increment(ReasonAtom, CmpTid), %% Cmp stat update
			log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, ReasonBin2}], DebugBid);

		{pass, Cr} ->
			{_, Config} = bidder_cmp:dirty_read_cmp(CmpTid, <<"config">>),
			{_, Rate} = bidder_cmp:dirty_read_cmp(CmpTid, <<"pacing_rate">>),
			Adomain = tk_maps:get([<<"adomain">>], Config, <<"">>),
			Bid = tk_maps:get([<<"bid">>, <<"bid">>], Config, 0),
			BidType = tk_maps:get([<<"bid">>, <<"type">>], Config, <<"random">>),
			BidFloor = tk_maps:get([<<"bidfloor">>], Cr, 0.0),
			Test = tk_maps:get([<<"test">>], BR),
			ModelBR = get_model_br(BR, Cr, Config),
			case tkb_bidder_model:calc_bid(BidType, ModelBR, Bid, BidFloor, Rate) of
				{no_bid, bidfloor} ->
					bidder_stats:increment(failed_bidfloor, CmpTid), %% Cmp stat update
					log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, <<"fail - bidfloor">>}], DebugBid);
				{no_bid, ctr_prediction} ->
					bidder_stats:increment(failed_ctr, CmpTid), %% Cmp stat update
					log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, <<"fail - ctr">>}], DebugBid);
				{no_bid, model_timeout} ->
					bidder_stats:increment(failed_model_timeout, CmpTid), %% Cmp stat update
					log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, <<"fail - model_timeout">>}], DebugBid);
				{no_bid, pacing_rate} ->
					bidder_stats:increment(failed_pacing_rate, CmpTid), %% Cmp stat update
					log_bid(BidId, [{<<"bid_cmp_", Cmp/binary>>, <<"fail - pacing_rate">>}], DebugBid);
				BidPrice -> tk_lib:echo1(price, BidPrice),
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
check_reason({[<<"device">>, <<"type">>], include}) ->
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


get_model_br(BR, Cr, CmpConfig) ->
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