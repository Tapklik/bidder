-module(campaign_parse_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
	all/0, groups/0,
	init_per_suite/1, end_per_suite/1,
	init_per_group/2, end_per_group/2
]).
-export([
	eunit_data/1,
	eunit_cmp/1
]).


-define(Cmp1, campaign_helper:campaign_1()).

-define(HOUR_OF_WEEK, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
	27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
	56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
	85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
	111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133,
	134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156,
	157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168]).


all() -> [
	{group, cmp1}
].

groups() -> [
	{cmp1, [], [
		eunit_data,
		eunit_cmp
	]}
].

init_per_suite(Config) ->
	Pid = spawn(fun ets_owner_proscess/0),
	Tables = [
		{regions, campaign_helper:regions()},
		{banner_sizes, campaign_helper:banner_sizes()},
		{users, campaign_helper:users()}
	],
	lists:foreach(
		fun({Name, Data}) ->
			ets:new(Name, [named_table, set, public, {read_concurrency, true}, {heir, Pid, []}]),
			ets:insert(Name, Data)
		end
		, Tables),
	Config.

end_per_suite(Config) ->
	Config.

init_per_group(cmp1, Config) ->
	Cmp = jsx:decode(?Cmp1, [return_maps]),
	Config ++ [{cmp, Cmp}].

end_per_group(cmp1, Config) ->
	lists:delete([cmp], Config).


%%%%%%%%%%%%%%%%%%%%%%
%%%    TESTS	   %%%
%%%%%%%%%%%%%%%%%%%%%%

eunit_data(_Config) ->
	?assertEqual(ets:lookup(regions, <<"ARE-FU">>), [{<<"ARE-FU">>, <<"Fujairah">>}]).


eunit_cmp(Config) ->
	Cmp = ?config(cmp, Config),
	Parsed = bidder_cmp_parser:parse_cmp(Cmp),
	ct:print("TEST: ~p", [Parsed]),
	?assertEqual(proplists:get_value(<<"id">>, Parsed), <<"53e292e0">>),
	?assertEqual(proplists:get_value(<<"status">>, Parsed), <<"running">>),
	%% filters_1
	Filters_1 = proplists:get_value(<<"filters_1">>, Parsed),
	?assertEqual(maps:get(<<"exchange">>, Filters_1), [1]),
	?assertEqual(maps:get(<<"bidmax">>, Filters_1), 3.1),
	?assertEqual(maps:get(<<"adomain">>, Filters_1), [<<"will.com">>]),
	?assertEqual(maps:get(<<"user">>, Filters_1), [2, 5, 10]),
	?assertEqual(maps:get(<<"device_type">>, Filters_1), 4),
	?assertEqual(maps:get(<<"hourofweek">>, Filters_1), ?HOUR_OF_WEEK),
	?assertEqual(maps:get(<<"country">>, Filters_1), [<<"USA">>, <<"ARE">>]),
	?assertEqual(maps:get(<<"creatives_type">>, Filters_1), [1069, 1059]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

ets_owner_proscess() ->
	receive
		stop -> exit(normal);
		_Any -> ets_owner_proscess()
	end.