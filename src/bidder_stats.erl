-module(bidder_stats).

-include("bidder_global.hrl").
-include("lager.hrl").
-include_lib("../lib/amqp_client/include/amqp_client.hrl").

-export([increment/2, reset_stats/1, send_stats/1]).


-record(state, {
	ts = 0
}).

-define(INTERVAL, 2000).
-define(METRICS, [
	<<"bid_requests">>,
	<<"bids">>,
	<<"imps">>,
	<<"clicks">>,
	<<"bids.failed.budget">>,
	<<"bids.failed.bidfloor">>,
	<<"bids.failed.hourofweek">>,
	<<"bids.failed.geo">>,
	<<"bids.failed.cat">>,
	<<"bids.failed.creative">>,
	<<"bids.failed.device">>,
	<<"bids.failed.user">>,
	<<"bids.failed.other">>
]).

-type bidder_stat() :: 	bid_request
| bid
| imp
| click
| failed_budget
| failed_bidfloor
| failed_hourofweek
| failed_geo
| failed_cat
| failed_creative
| failed_user
| failed_device
| failed_other.

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%


-spec(increment(Stat :: bidder_stat(), CmpTid :: integer()) -> any()).
increment(Stat, CmpTid) ->
	internal_increment(Stat, CmpTid).


send_stats(CmpTid) ->
	CmpList = ets:tab2list(CmpTid),
	AccId = proplists:get_value(<<"acc">>, CmpList),
	Cmp = proplists:get_value(<<"cid">>, CmpList),
	[statsderl:increment(<<"cmp.", AccId/binary, ".", Cmp/binary, ".", M/binary>>, proplists:get_value(M, CmpList, 0), 1)
		|| M <- ?METRICS].


reset_stats(CmpTid) ->
	InsertList = [{M, 0} || M <- ?METRICS],
	ets:insert(CmpTid, InsertList).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

internal_increment(bid_request, CmpTid) ->
	ets:update_counter(CmpTid, <<"bid_requests">>, 1);
internal_increment(bid, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids">>, 1);
internal_increment(imp, CmpTid) ->
	ets:update_counter(CmpTid, <<"imps">>, 1);
internal_increment(click, CmpTid) ->
	ets:update_counter(CmpTid, <<"clicks">>, 1);
internal_increment(failed_budget, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.budget">>, 1);
internal_increment(failed_bidfloor, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.bidfloor">>, 1);
internal_increment(failed_hourofweek, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.hourofweek">>, 1);
internal_increment(failed_geo, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.geo">>, 1);
internal_increment(failed_cat, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.cat">>, 1);
internal_increment(failed_creative, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.creative">>, 1);
internal_increment(failed_device, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.device">>, 1);
internal_increment(failed_user, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.user">>, 1);
internal_increment(failed_other, CmpTid) ->
	ets:update_counter(CmpTid, <<"bids.failed.other">>, 1).

