-module(tkb_bids_filter).

-include("global.hrl").
-include("lager.hrl").


-export([filter_bid/2]).

-export([
	include/3,
	exclude/3,
	equal/3]).

-export([get_hourofweek/3]).

-type map_path() :: [binary()] | binary().
-type filter_action() :: pass | fail.
-type br() :: map().
-type creative() :: map().
-type filter() :: map().
-type tid() :: integer().


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% This is the main function used to filter bid requests.
%%
%% For now it consists of 3 main passes:
%% 	1) Filters_1: usually simple and fast filters to eliminate unqualified campaigns
%% 	2) Select creatives: in this pass, we look for a matching creative (or fail)
%% 	3) Filters_2: these are the more complex filters that might use regex and string manipulation
%%
-spec filter_bid(tid(), br()) -> {fail, atom()} | {pass, creative()}.
filter_bid(CmpTid, BR) ->
	Filtering = ?ENV(filtering, false),
	case Filtering of
		false ->
			%% no need to filter. Filtering is disabled
			select_creative(CmpTid, BR, pass);
		_ ->
			%% Filtering is enabled:
			R = check_pacing_rate(CmpTid),
			F1 = filter_bid_1(CmpTid, BR, R),
			select_creative(CmpTid, BR, F1)
	end.

%%%%%%%%%%%%%%%%%%%%%%
%%%    PRIVATE     %%%
%%%%%%%%%%%%%%%%%%%%%%

check_pacing_rate(CmpTid) ->
	case bidder_cmp:dirty_read_cmp(CmpTid, <<"pacing_rate">>) of
		{<<"pacing_rate">>, Rate} ->
			Rate >= rand:uniform();
		_ ->
			%% cmp config not found
			{fail, config_not_found}
	end.

-spec filter_bid_1(tid(), br(), boolean()) -> pass | fail.
filter_bid_1(_, _, false) ->
	{fail, pacing_rate};
filter_bid_1(CmpTid, BR, true) ->
	case bidder_cmp:dirty_read_cmp(CmpTid, <<"filters">>) of
		{<<"filters">>, Filters} ->
			filter_internal(filters_1(), Filters, BR);
		_ ->
			%% cmp config not found
			{fail, config_not_found}
	end.

-spec select_creative(tid(), br(), F1 :: pass | fail) -> creative().
select_creative(_, _, {fail, R}) ->
	{fail, R};
select_creative(CmpTid, BR, _F1) ->
	case bidder_cmp:dirty_read_cmp(CmpTid, <<"creatives">>) of
		{<<"creatives">>, Creatives} ->
			MatchingCreatives = lists:foldl(
				fun(Creative, Acc) ->
					%% TODO change later to combine banner and html5; for now treat as same
					C1 = case tk_maps:get([<<"class">>], Creative) of
							 <<"html5">> -> <<"banner">>;
							 C -> C
						 end,
					C2 = tk_maps:get([<<"imp">>, <<"class">>], BR),
					Imp = tk_maps:get([<<"imp">>], BR),
					case C1 == C2 of
						%% Banner
						true when C1 == <<"banner">> ->
							case filter_internal(creatives_banner(), Creative, Imp) of
								{fail, _} -> Acc;
								pass -> [Creative | Acc]
							end;
						%% Video
						true when C1 == <<"video">> ->
							Acc;
						%% Native
						true when C1 == <<"native">> ->
							Acc;
						%% Other
						_ ->
							Acc
					end
				end
				, [], Creatives),
			case length(MatchingCreatives) of
				0 -> {fail, no_matching_creative};
				1 -> {pass, hd(MatchingCreatives)};
				L -> {pass, lists:nth(rand:uniform(L), MatchingCreatives)}
			end;
		_ ->
			%% cmp config not found
			{fail, creative_config_not_found}
	end.

-spec filter_bid_2(tid(), br()) -> pass | fail.
filter_bid_2(CmpTid, BR) ->
	pass.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @hidden
%%
%% Goes over the FilterConfig list which is identified below or later through a config 
%% 	file, and checks if each filter passes or fails.
%%
-spec filter_internal(atom(), filter(), Map :: creative() | br()) -> filter_action().
filter_internal(FilterFun, FilterConfig, Map) ->
	filter_internal(FilterFun, FilterConfig, Map, pass).
filter_internal(_, _, _, {fail, R}) -> {fail, R};
filter_internal([], _, _, Result) -> Result;
filter_internal([{P1, {Fun, Default}, P2} | T], FilterConfig, Map, _Result) ->
	case filter_internal2(Fun, P1, P2, FilterConfig, Map, Default) of
		pass ->
			filter_internal(T, FilterConfig, Map, pass);
		fail ->
			filter_internal(T, FilterConfig, Map, {fail, {P1, Fun}})
	end.

filter_internal2(Fun, Path1, Path2, FilterConfig, Map, Default) ->
	%% V1 usually comes from campaigns
	V1 = case Path1 of
			 {mfa, F1, A1} ->
				 ?MODULE:F1(A1, FilterConfig, Map);
			 P1 when is_list(P1) ->
				 lookup(P1, FilterConfig)
		 end,
	%% V2 usually comes from bid request
	V2 = case Path2 of
			 {mfa, F2, A2} ->
				 ?MODULE:F2(A2, FilterConfig, Map);
			 P2 when is_list(P2) ->
				 lookup(P2, Map)
		 end,
	?MODULE:Fun(V1, V2, Default).


%%
%% @hidden
%%
%% Include logic:
%%
%% 		Cmp (V1)		Map (V2)		Possible outcome
%% 		========		========		================
%%		defined			defined			pass | fail
%%		defined			not defined		Default :: pass | fail
%%		not defined		defined			pass
%%		not defined		not	defined		pass
%%
-spec include(any(), any(), filter_action()) -> filter_action().
include(V1, V2, Default) ->
	case [{X, Y} || X <- V1, Y <- V2, X == Y] of
		[] when V1 == [] -> pass;
		[] when V2 == [] -> Default;
		[] -> fail;
		_ -> pass
	end.


%%
%% @hidden
%%
%% Exclude logic:
%%
%% 		Cmp (V1)		Map (V2)		Possible outcome
%% 		========		========		================
%%		defined			defined			pass | fail
%%		defined			not defined		pass
%%		not defined		defined			pass
%%		not defined		not	defined		pass
%%
-spec exclude(any(), any(), filter_action()) -> filter_action().
exclude(V1, V2, _Default) ->
	case [{X, Y} || X <- V1, Y <- V2, X == Y] of
		[] when V1 == [] -> pass;
		[] when V2 == [] -> pass;
		[] -> pass;
		_ -> fail
	end.

%%
%% @hidden
%%
%% Equal logic:
%%
%% 		Cmp (V1)		Map (V2)		Possible outcome
%% 		========		========		================
%%		=/= 0			=/= 0			pass | fail
%%		= 0		or 		= 0				Default
%%
-spec equal(any(), any(), filter_action() | any()) -> pass | fail.
equal(V1, V2, Default) ->

	case V1 == V2 of
		false when (V1 == undefined orelse v2 == undefined) -> Default;
		false -> fail;
		true -> pass
	end.


-spec lookup(map_path(), map()) -> list().
lookup(Path, Map) when is_list(Path) == false ->
	lookup([Path], Map);
lookup(Path, Map) ->
	case tk_maps:get(Path, Map) of
		{error, _} -> [];
		Value ->
			case is_list(Value) of
				true -> Value;
				false -> [Value]
			end
	end.


get_hourofweek(_Args, FilterConfig, BR) ->
	HourOfWeekUTC = tk_maps:get([<<"hourofweek">>], BR),
	Diff = tk_maps:get([<<"timezone_diff">>], FilterConfig),
	case HourOfWeekUTC + Diff of
		H when H =< 0 ->
			[168 + H];
		H when H > 168 ->
			[H - 168];
		H ->
			[H]
	end.

%%%%%%%%%%%%%%%%%%%%%%
%%% CONFIGURATIONS %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @hidden
%% Simple filters which are the fastest go first.
%% The format is:
%% 	 path_to_value(Cmp) -- {op, default} -- path_to_value(BR)
%%
filters_1() ->
	[
		{[<<"exchange">>], {include, fail}, [<<"exchange">>]},
		{[<<"adomain">>], {exclude, pass}, [<<"badv">>]},
		{[<<"device">>,<<"type">>], {include, pass}, [<<"device">>, <<"type">>]},
		{[<<"user">>, <<"gender">>], {include, pass}, [<<"user">>, <<"gender">>]},
		{[<<"budget">>, <<"hourofweek">>], {include, fail}, {mfa, get_hourofweek, []}},
		{[<<"cat">>], {exclude, pass}, [<<"bcat">>]},
		{[<<"cat">>], {include, pass}, [<<"cat">>]},
		{[<<"country">>], {include, fail}, [<<"geo">>, <<"country">>]}
	].

filters_2() ->
	[

	].

creatives_banner() ->
	[
		{[<<"dim">>], {include, fail}, [<<"dim">>]},
		{[<<"pos">>], {equal, pass}, [<<"pos">>]},
		{[<<"expdir">>], {equal, pass}, [<<"expdir">>]},
		{[<<"attr">>], {exclude, pass}, [<<"battr">>]},
		{[<<"type">>], {exclude, pass}, [<<"btype">>]}
	].