-module(bidder_stats).

-behaviour(gen_server).

-include("bidder_global.hrl").
-include("lager.hrl").
-include_lib("../lib/amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	ts = 0
}).

-define(INTERVAL, 2000).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%%
start_link() ->
	gen_server:start_link(?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @private
%% Starts the gen_server loop by sending an {interval} msg to initiate.
%%
%% This gen_server checks when a new time slot (ts) has started to collect the stats
%% 	from the bidders and publish then to RMQ.
%%
init([]) ->
	process_flag(trap_exit, true),
	erlang:send_after(?INTERVAL, self(), {interval}),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

%%
%% @private
%%
handle_info({interval}, State) ->
	Ts0 = State#state.ts,
	{Ts1, _T} = get_current_ts(),
	case Ts1 == Ts0 of
		true ->
			ok;
		false ->
			%% Async start of process to collect and publish stats
			Pid = spawn(fun collect_and_publish_stats/0),
			Pid ! {ts, Ts0}
	end,
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{ts = Ts1}};
handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

%%
%% @private
%% Error in case of exit
%%
terminate(_Reason, _State) ->
	?ERROR("BIDDER: Stats collection module has stopped on node ~p", [node()]).

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

collect_and_publish_stats() ->
	receive
		%% Ignore first time slot since no previous data exists
		{ts, 0} ->
			ok;
		%% Start collecting data
		{ts, Ts0} ->
			collect_and_publish_stats(Ts0)
	after
		100 -> ok
	end.

collect_and_publish_stats(Ts0) ->
	CmpPids = [Pid || {_, Pid, _, _} <- ets:tab2list(cmp_list)],
	StatsList = lists:map(
		fun(P) ->
			{ok, Stats} = gen_server:call(P, {get_and_reset_stats}), tk_lib:echo1(stats, Stats),
			Stats
		end
		, CmpPids),
	Stats = #{
		<<"node">> => node(),
		<<"time_slot">> => Ts0,
		<<"stats">> => StatsList
	},
	StatsJson = jsx:encode(Stats),
	?INFO("BIDDER_STATS: Bidder stats on node ~p (ts: ~p): ~p", [node(), Ts0, StatsJson]),
	rmq:publish(cmp_stats, jsx:encode(Stats)).

get_current_ts() ->
	{ok, TsLength} = application:get_env(ts_length),
	{_, Time} = calendar:now_to_universal_time(erlang:timestamp()),
	TimeSinceMidnight = calendar:time_to_seconds(Time),
	Ts = trunc(TimeSinceMidnight / TsLength),
	DayTs = trunc(24 * 60 * 60 / TsLength),
	{Ts, DayTs}.