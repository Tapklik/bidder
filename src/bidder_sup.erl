-module(bidder_sup).

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
	RmqSup = #{
		id => rmq_sup,
		start => {rmq_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [rmq_sup]
	},
	Pooler = #{
		id => bidder_pooler_sup,
		start => {bidder_pooler_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type =>supervisor,
		modules => [bidder_pooler_sup]
	},
	%% TODO Not started
	BidderData = #{
		id => bidder_data,
		start => {bidder_data, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [bidder_data]
	},
	%% TODO Not started
	BidderStats = #{
		id => bidder_stats,
		start => {bidder_stats, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [bidder_stats]
	},
	VMServer = #{
		id => vm,
		start => {vm, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [vm]
	},
	Children = [CmpSup, BidderSup, RmqSup, Pooler, VMServer],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
