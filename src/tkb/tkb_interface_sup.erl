-module(tkb_interface_sup).

-behaviour(supervisor).

-include("bidder_global.hrl").
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
	CmpGS = #{
		id => tkb_interface,
		start => {tkb_interface, start_link, []},
		restart => transient,
		shutdown => 200,
		type => worker,
		modules => [tkb_interface]
	},
	Children = [CmpGS],
	RestartStrategy = {simple_one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
