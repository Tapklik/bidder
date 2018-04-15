-module(tkb_sup).

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
	BidderInterfaceSup = #{
		id => tkb_interface_sup,
		start => {tkb_interface_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [tkb_interface_sup]
	},
	Children = [BidderInterfaceSup],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
