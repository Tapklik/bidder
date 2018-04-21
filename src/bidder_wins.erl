-module(bidder_wins).

-include("global.hrl").
-include("lager.hrl").
-include_lib("../lib/amqp_client/include/amqp_client.hrl").

-export([
	save_bid/4,
	mark_win/1
]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

save_bid(TimeStamp, BidId, BR, RSP) ->
	SaveBid = ?BIDS_SAVE_PCTG >= rand:uniform(),
	{AccId, Cmp, _Crid, _BidPrice} = check_rsp_output(RSP),
	Data = #{
		<<"timestamp">> => TimeStamp,    	% time stamp (5 mins)
		<<"bid_id">> => BidId,          	% id
		<<"acc">> => AccId,                	% account id
		<<"cmp">> => Cmp,                	% campaign id
		<<"br">> => BR,                		% br
		<<"rsp">> => RSP,            		% creative id
		<<"include">> => SaveBid        	% include
	},
	cache:put(bids_cache, BidId, Data).


mark_win(WinBin) ->
	WinMap = binary_to_term(WinBin),
	#{
		<<"timestamp">> := _TimeStamp,        	% time stamp (5 mins)
		<<"bid_id">> := BidId,            		% id
		<<"acc">> := AccId,            			% account id
		<<"cmp">> := Cmp,                    	% campaign id
		<<"crid">> := Crid,                    	% creative id
		<<"win_price">> := WinPrice ,           % win price
		<<"spend">> := Spend            		% spend
	} = WinMap,
	%% Updating CMP counters -----------------
	case ets:lookup(cmp_list, Cmp) of
		[] -> ok;
		[{_, _, _, CmpTid, _} | _] -> ets:update_counter(CmpTid, <<"imps">>, 1)
	end,
	%% ---------------------------------------
	case cache:get(bids_cache, BidId) of
		undefined ->
			?ERROR("BIDDER (~p): No matching bid found for win! (Acc: ~p, Cmp: ~p, BidId: ~p)", [AccId, Cmp, BidId]),
			{ok, no_bid_in_cache};
		Bid when is_map(Bid) ->
			?INFO("BIDDER (~p): Received win! (Acc: ~p, Cmp: ~p, BidId: ~p)", [?ENV(app_id), AccId, Cmp, BidId]),
			Data = Bid#{
				<<"crid">> => Crid,
				<<"win_price">> => WinPrice,
				<<"spend">> => Spend
			},
			publish_to_stream(?BIDS_STREAM_TOPIC, BidId, Data)
	end,
	{ok, marked}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%


publish_to_stream(Topic, BidId, Load0) ->
	spawn(
		fun() ->
			case ?ENV(stream_enabled) of
				true ->
					Load = base64:encode(jsx:encode(Load0)),
					kinetic:put_record([
						{<<"Data">>, Load},
						{<<"PartitionKey">>, BidId},
						{<<"StreamName">>, Topic}
					]);
				_ ->
					ok
			end
		end).


check_rsp_output(RSP) ->
	AccId = tk_maps:get([<<"acc">>], RSP),
	Cmp = tk_maps:get([<<"cid">>], RSP),
	Crid = tk_maps:get([<<"creative">>, <<"crid">>], RSP),
	BidPrice = tk_maps:get([<<"price">>], RSP),
	{AccId, Cmp, Crid, BidPrice}.