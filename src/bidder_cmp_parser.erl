-module(bidder_cmp_parser).

-include("bidder_global.hrl").
-include("lager.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([parse_cmp/1]).

-export([
	parse_filters/2
]).

-export([
	find_if_exists/2,
	find_if_exists/3,
	find_or_invalid/2,
	find_all/2,
	find_either/2,
	try_ets_lookup/2
]).

-export([
	get_hourofweek_plan/2
]).

-export([
	cmp_parser_filters/0,
	cmp_parser_budget/0
]).

%% TYPES
-type cmp() :: #{binary() => any()}.

-type filter() :: #{binary() => any()}.

-type map_path() :: [binary()] | binary().

-type imp() :: #{binary() => any()}.

-export_type([cmp/0, filter/0, imp/0, map_path/0]).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec parse_cmp(cmp()) -> invalid_cmp | [{binary(), any()}].
parse_cmp(Cmp) ->
	parse_cmp(cmp_parser(), Cmp, []).
parse_cmp(_, _, invalid_cmp) -> invalid_cmp;
parse_cmp([], _Cmp, Acc) -> Acc;
parse_cmp([{Parameter, Fun, Args} | T], Cmp, Acc) ->
	Acc2 = case ?MODULE:Fun(Args, Cmp) of
			   invalid -> invalid_cmp;
			   ignore -> Acc;
			   {kv, Key, Value} -> [{Key, Value} | Acc];
			   Value -> [{Parameter, Value} | Acc]
		   end,
	parse_cmp(T, Cmp, Acc2).


%%%%%%%%%%%%%%%%%%%%%%
%%%    PRIVATE 	   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec parse_filters(atom(), cmp()) -> invalid_cmp | filter().
parse_filters(FiltersFun, Cmp) ->
	parse_filters(?MODULE:FiltersFun(), Cmp, #{}).
parse_filters(_, _, invalid_cmp) -> invalid_cmp;
parse_filters([], _Cmp, Acc) -> Acc;
parse_filters([{Parameter, Fun, Args} | T], Cmp, Acc) ->
	Acc2 = case ?MODULE:Fun(Args, Cmp) of
			   invalid -> invalid_cmp;
			   ignore -> Acc;
			   {kv, Key, Value} -> maps:put(Key, Value, Acc);
			   Value -> maps:put(Parameter, Value, Acc)
		   end,
	parse_filters(T, Cmp, Acc2).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec find_if_exists(map_path(), map()) -> Default :: any() | any().
find_if_exists(Path, Map) ->
	find_if_exists(Path, Map, <<"">>).
find_if_exists(Path, Map, Default) when is_list(Path) == false ->
	find_if_exists([Path], Map, Default);
find_if_exists(Path, Map, Default) ->
	case tk_maps:get(Path, Map) of
		{error, _} -> Default;
		Value -> Value
	end.

-spec find_or_invalid(map_path(), map()) -> invalid | any().
find_or_invalid(Path, Map) when is_list(Path) == false ->
	find_or_invalid([Path], Map);
find_or_invalid(Path, Map) ->
	case tk_maps:get(Path, Map) of
		{error, _E} -> invalid;
		Value -> Value
	end.

-spec find_either([{any(), map_path()}], map()) -> invalid | {kv, any(), any()}.
find_either([], _Map) ->
	invalid;
find_either([{Key, Path} | T], Map) ->
	case tk_maps:get(Path, Map) of
		{error, _} -> find_either(T, Map);
		Value -> {kv, Key, Value}
	end.

-spec find_all([tuple()], map()) -> list().
find_all(Paths, Map) ->
	find_all(Paths, Map, []).
find_all([], _Map, Acc) ->
	Acc;
find_all([{_Key, Path} | T], Map, Acc) ->
	case tk_maps:get(Path, Map) of
		{error, _} -> find_all(T, Map, Acc);
		Value -> find_all(T, Map, Acc ++ [Value])
	end.

get_hourofweek_plan(Path, Map) ->
	Plan = tk_maps:get(Path, Map),
	HowMatrix = ets:tab2list(hourofweek_matrix),
	get_hourofweek_plan2(Plan, HowMatrix).

get_hourofweek_plan2(Plan, Matrix) ->
	get_hourofweek_plan2(Plan, Matrix, 0, 1, []).
get_hourofweek_plan2(<<"">>, _, _, _, Acc) -> Acc;
get_hourofweek_plan2(<<P:8, Rest/binary>>, Matrix, Day, T, Acc) when P == 32 ->
	get_hourofweek_plan2(Rest, Matrix, Day + 1, 1, Acc);
get_hourofweek_plan2(<<P:8, Rest/binary>>, Matrix, Day, T, Acc) ->
	Hour = case P of
			   48 -> [];
			   49 ->
				   {H0, H1} = proplists:get_value(T, Matrix),
				   Hour0 = Day * 24 + H0,
				   get_hours_list(Hour0, H1 - H0)
		   end,
	get_hourofweek_plan2(Rest, Matrix, Day, T + 1, Acc ++ Hour).

get_hours_list(H, N) -> get_hours_list(H, N, []).
get_hours_list(_, 0, Acc) -> Acc;
get_hours_list(H, N, Acc) ->
	get_hours_list(H, N - 1, [H + N | Acc]).


%% @hidden
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, <<"">>).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[{_, Val} | _] -> Val;
		[] -> Default
	end.

%% @hidden
cmp_parser() ->
	[
		{<<"id">>, find_or_invalid, [<<"id">>]},
		{<<"status">>, find_or_invalid, [<<"status">>]},
		{<<"creatives">>, find_or_invalid, [<<"creatives">>, <<"data">>]},
		{<<"filters">>, parse_filters, cmp_parser_filters}
	].

%% @hidden
cmp_parser_filters() ->
	[
		{<<"exchange">>, find_or_invalid, [<<"exchange">>, <<"data">>]},
		{<<"bid">>, find_or_invalid, [<<"bid">>]},
		{<<"adomain">>, find_or_invalid, [<<"adomain">>]},
		{<<"device">>, find_or_invalid, [<<"device">>, <<"data">>]},
		{<<"user">>, find_or_invalid, [<<"user">>, <<"data">>]},
		{<<"budget">>, parse_filters, cmp_parser_budget},
		{<<"cat">>, find_or_invalid, [<<"cat">>, <<"data">>]},
		{<<"geo">>, find_or_invalid, [<<"geo">>, <<"data">>]},
		{<<"country">>, find_or_invalid, [<<"geo">>, <<"data">>, [<<"country">>]]}
	].

cmp_parser_budget() ->
	[
		{<<"type">>, find_or_invalid, [<<"budget">>, <<"data">>, <<"type">>]},
		{<<"hourofweek">>, get_hourofweek_plan, [<<"budget">>, <<"data">>, <<"pacing">>]},
		{<<"amount">>, find_or_invalid, [<<"budget">>, <<"data">>, <<"amount">>]}
	].
