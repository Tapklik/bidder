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
			ModelBR = get_model_br(BR, Cr, {Cmp, Config}), tk_lib:echo1(model_br, ModelBR),
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


get_model_br(BR, Cr, {Cmp, CmpConfig}) ->
	[Dimension | _] = tk_maps:get([<<"dim">>], Cr),
	CatBinList = cat_to_bin(tk_maps:get([<<"cat">>], BR)),
	#{
		<<"bid-id">> => tk_maps:get([<<"id">>], BR),
		<<"device_make">> => tk_maps:get([<<"device">>, <<"make">>], BR, <<"">>),
		<<"device_model">> => tk_maps:get([<<"device">>, <<"model">>], BR, <<"">>),
		<<"device_os">> => tk_maps:get([<<"device">>, <<"os">>], BR, <<"">>),
		<<"device_type">> => tk_maps:get([<<"device">>, <<"type">>], BR, 5),
		<<"device_ua">> => tk_maps:get([<<"device">>, <<"ua">>], BR, <<"">>),
		<<"city">> => tk_maps:get([<<"geo">>, <<"city">>], BR, <<"">>),
		<<"country">> => tk_maps:get([<<"geo">>, <<"country">>], BR, <<"">>),
		<<"hour_of_week">> => tk_maps:get([<<"hourofweek">>], BR, 0),
		<<"position">> => tk_maps:get([<<"imp">>, <<"pos">>], BR, 0),
		<<"content_rating">> => tk_maps:get([<<"publisher">>, <<"contentrating">>], BR),
		<<"content_language">> => tk_maps:get([<<"publisher">>, <<"language">>], BR),
		<<"amp">> => tk_maps:get([<<"publisher">>, <<"amp">>], BR, 0),
		<<"mobile">> => tk_maps:get([<<"publisher">>, <<"mobile">>], BR, 0),
		<<"page">> => tk_maps:get([<<"publisher">>, <<"domain">>], BR, <<"">>),
		<<"publisher_type">> => tk_maps:get([<<"publisher">>, <<"type">>], BR, <<"site">>),
		<<"publisher_country">> => tk_maps:get([<<"publisher">>, <<"country">>], BR, <<"">>),
		<<"adomain">> => tk_maps:get([<<"adomain">>], CmpConfig, <<"">>),
		<<"class">> => tk_maps:get([<<"class">>], Cr, <<"banner">>),
		<<"cmp">> => Cmp,
		<<"crid">> => tk_maps:get([<<"crid">>], Cr),
		<<"click">> => 0,
		<<"dimension">> => Dimension,
		<<"data-segment">> => <<"[]">>,
		<<"viewability">> => tk_maps:get([<<"imp">>, <<"metric">>, <<"viewability">>], BR, 0),
		<<"session_depth">> => tk_maps:get([<<"imp">>, <<"metric">>, <<"session_depth">>], BR, 0),
		<<"categories">> => CatBinList,
		<<"click_through_rate">> => tk_maps:get([<<"imp">>, <<"metric">>, <<"click_through_rate">>], BR, 0),
		<<"video_completion_rate">> => tk_maps:get([<<"imp">>, <<"metric">>, <<"video_completion_rate">>], BR, 0)
	}.


cat_to_bin(CatList) ->
	CB = cat_to_bin(CatList, <<>>),
	<<"[", CB/binary, "]">>.
cat_to_bin([], Acc) -> Acc;
cat_to_bin([C | T], <<>>) ->
	cat_to_bin(T, <<C/binary>>);
cat_to_bin([C | T], Acc) ->
	cat_to_bin(T, <<Acc/binary, ",", C/binary>>).