-module(bidder_cmp_sup).

-behaviour(supervisor).

-include("global.hrl").
-include("lager.hrl").


-export([start_link/0]).
-export([init/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	ets:new(cmp_list, [public, named_table, set, {read_concurrency, true}]),
	CmpGS = #{
		id => bidder_cmp,
		start => {bidder_cmp, start_link, []},
		restart => transient,
		shutdown => 50,
		type => worker,
		modules => [bidder_cmp]
	},
	Children = [CmpGS],
	RestartStrategy = {simple_one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
