-module(bidder_sup).

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
	CmpSup = #{
		id => bidder_cmp_sup,
		start => {bidder_cmp_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type =>supervisor,
		modules => [bidder_cmp_sup]
	},
	BidderSup = #{
		id => tkb_sup,
		start => {tkb_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type =>supervisor,
		modules => [tkb_sup]
	},
	BidderModel = #{
		id => bidder_model,
		start => {bidder_model, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [bidder_model]
	},
	VMServer = #{
		id => vm,
		start => {vm, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [vm]
	},
	%% Time server gen_server
	TimeServer = #{
		id => time_server,
		start => {time_server, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [time_server]
	},
	Children = [CmpSup, BidderSup, TimeServer, BidderModel, VMServer],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
