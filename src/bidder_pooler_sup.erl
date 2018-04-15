-module(bidder_pooler_sup).

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
	PoolerSup = #{
		id => pooler_sup,
		start => {pooler_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [pooler_sup]
	},
	Children = [PoolerSup],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
