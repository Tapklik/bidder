-module(bidrequest_filter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
	all/0, groups/0,
	init_per_suite/1, end_per_suite/1,
	init_per_group/2, end_per_group/2
]).
-export([
	eunit_br/1
]).

-define(FILTERING, true).

-define(Cmp1, campaign_helper:campaign_1()).

-define(BR1, bidrequest_helper:bid_request_1()).



all() -> [
	{group, br1}
].

groups() -> [
	{br1, [], [
		eunit_br
	]}
].

init_per_suite(Config) ->
	Pid = spawn(fun ets_owner_proscess/0),
	ets:new(cmp2pid, [named_table, set, public, {read_concurrency, true}, {heir, Pid, []}]),

	Config ++ [{owner, Pid}].

end_per_suite(Config) ->
	Config.

init_per_group(br1, Config) ->
	Pid = ?config(owner, Config),
	Cmp0 = jsx:decode(?Cmp1, [return_maps]),
	CmpConfig = bidder_cmp_parser:parse_cmp(Cmp0),
	Tid = ets:new(cmp, [set, public, {read_concurrency, true}, {heir, Pid, []}]),
	ets:insert(Tid, CmpConfig),
	Config ++ [{tid1, Tid}].

end_per_group(cmp1, Config) ->
	lists:delete([cmp], Config).


%%%%%%%%%%%%%%%%%%%%%%
%%%    TESTS	   %%%
%%%%%%%%%%%%%%%%%%%%%%

eunit_br(Config) ->
	Cmp = ?config(cmp, Config),
	{C, B} = tkb_bids_filter:filter_bid(Cmp, ?BR1),
	ct:print("CMP: ~n ~p ~n ~n BR: ~n ~p ~n", [C , B]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

ets_owner_proscess() ->
	receive
		stop -> exit(normal);
		_Any -> ets_owner_proscess()
	end.