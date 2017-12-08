-module(bidder_cmp).

-behaviour(gen_server).

-include("bidder_global.hrl").
-include("lager.hrl").
-include_lib("../lib/amqp_client/include/amqp_client.hrl").

-export([start_link/0, start_link/3]).

-export([load_cmp_config/1, dirty_read_cmp/2]).
-export([start_cmp/3, stop_cmp/1, check_cmp/1]).


-export([get_cmp_stats/1]).
-export([get_all_cmps/0, get_and_reset_all_cmps_stats/0]).

-export([set_pacing_rate/1]).

-export([save_bert_file/1]).
-export([try_ets_lookup/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	cmp,
	tid,
	timestamp,
	hash,
	pacing_timestamp
}).

-define(INTERVAL, 5 * 1000).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% This simple_one_to_one gen_server is responsible for CRUD operations for campaigns
%% 	to ETS tables.
%%
%% Three ETS tables are used and updated:
%% 	- cmp2pid: simple table to store Pid of this gen_server relevant to Cmp ID table
%% 	- cmp_list: stores a list of all running campaigns with their current temp status
%% 	- cmp: table that takes random ID number which is stored in cmp2pid
%%
start_link() ->
	gen_server:start_link(?MODULE, [], []).
start_link(Cmp, CmpConfig, CmpHash) ->
	gen_server:start_link(?MODULE, [Cmp, CmpConfig, CmpHash], []).


load_cmp_config(ConfigJson) ->
	#{
		<<"cmp">> := Cmp,
		<<"hash">> := Hash,
		<<"config">> := CmpConfig
	} = jsx:decode(ConfigJson, [return_maps]),
	case try_ets_lookup(cmp_list, Cmp) of
		not_found ->
			{ok, _} = start_cmp(Cmp, CmpConfig, Hash);
		{_, _, Pid, _, _} ->
			case gen_server:call(Pid, {get_cmp_hash}) of
				{ok, Hash} ->
					gen_server:call(Pid, {reset_timeout});
				_ ->
					stop_cmp(Cmp),
					{ok, _} = start_cmp(Cmp, CmpConfig, Hash)
			end
	end,
	{ok, loaded}.

start_cmp(Cmp, CmpConfig, CmpHash) ->
	case try_ets_lookup(cmp_list, Cmp) of
		not_found ->
			{ok, _} = supervisor:start_child(bidder_cmp_sup, [Cmp, CmpConfig, CmpHash]),
			timer:sleep(10),
			check_cmp(Cmp);
		{_, _, Pid, _, _} ->
			gen_server:cast(Pid, {reset_timeout}),
			{ok, already_started}
	end.


dirty_read_cmp(CmpTid, Key) ->
	try_ets_lookup(CmpTid, Key, not_found).

stop_cmp(Cmp) ->
	case try_ets_lookup(cmp_list, Cmp) of
		not_found ->
			{error, not_found};
		{_, _, Pid, _, _} ->
			gen_server:cast(Pid, {stop_normal}),
			timer:sleep(10),
			check_cmp(Cmp)
	end.

check_cmp(Cmp) ->
	case try_ets_lookup(cmp_list, Cmp) of
		not_found ->
			{ok, not_found};
		_ ->
			{ok, running}
	end.

get_cmp_stats(Cmp) ->
	case try_ets_lookup(cmp_list, Cmp) of
		not_found ->
			{ok, #{
				<<"cmp">> => Cmp,
				<<"status">> => <<"not_found">>
			}};
		{_, _, Pid, _, _} ->
			gen_server:call(Pid, {get_stats})
	end.

get_all_cmps() ->
	[Cmp || {Cmp, _, _, _, _} <- ets:tab2list(cmp_list)].

get_and_reset_all_cmps_stats() ->
	CmpPids = [Pid || {_, _, Pid, _, _} <- ets:tab2list(cmp_list)],
	Stats = lists:map(
		fun(P) ->
			{ok, Stats} = gen_server:call(P, {get_and_reset_stats}),
			Stats
		end
		, CmpPids),
	#{
		<<"node">> => node(),
		<<"time_slot">> => ts,
		<<"stats">> => Stats
	}.

set_pacing_rate(PacingJson) ->
	Pacing = jsx:decode(PacingJson, [return_maps]),
	#{
		<<"cmp">> := Cmp,
		<<"pacing_rate">> := Rate
	} = Pacing,
	case try_ets_lookup(cmp_list, Cmp) of
		not_found ->
			{ok, not_fount};
		{_, _, Pid, _, _} ->
			gen_server:call(Pid, {set_pacing_rate, Rate})
	end.

save_bert_file(FileBin) ->
	[{Filename1, _Content} | _] = binary_to_term(FileBin),
	Filename2 = atom_to_list(Filename1),
	case file:write_file(?DATA_PATH ++ Filename2 ++ ".bert", FileBin) of
		ok ->
			?INFO("RMQ: File ~p is received and saved", [Filename2]),
			{ok, saved};
		{error, E} ->
			?ERROR("RMQ: Error in receiving or saving file ~p. (Error: ~p) ", [Filename2, E]),
			{error, E}
	end.

%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @private
%% Starts the gen_server and initiates through inserting proper entries to ETS tables.
%%	As well, starts a periodic {timeout} message.
%%
%% 	PS: this is a blocking init process because we need process started before updating Cmp
%%
init([Cmp, CmpConfig, CmpHash]) ->
	process_flag(trap_exit, true),
	Tid = ets:new(cmp, [public, set]),
	%% Insert initial counts for cmp stats
	#{
		<<"filters">> := Filters,
		<<"creatives">> := Creatives,
		<<"status">> := Status,
		<<"id">> := Cid,
		<<"config">> := Config,
		<<"acc">> := AccId
	} = CmpConfig,
	ets:insert(Tid, [
		{<<"filters">>, Filters},
		{<<"creatives">>, Creatives},
		{<<"status">>, Status},
		{<<"acc">>, AccId},
		{<<"cid">>, Cid},
		{<<"config">>, Config},
		{<<"hash">>, CmpHash},
		{<<"pacing_rate">>, 1.0}
	]),
	ets:insert(Tid, [
		{<<"bid_requests">>, 0},
		{<<"bids">>, 0},
		{<<"imps">>, 0},
		{<<"clicks">>, 0},
		{<<"bids.success">>, 0},
		{<<"bids.failed.budget">>, 0},
		{<<"bids.failed.bidfloor">>, 0},
		{<<"bids.failed.hourofweek">>, 0},
		{<<"bids.failed.geo">>, 0},
		{<<"bids.failed.cat">>, 0},
		{<<"bids.failed.creative">>, 0},
		{<<"bids.failed.user">>, 0},
		{<<"bids.failed.device">>, 0},
		{<<"bids.failed.other">>, 0}
	]),
	%% Global cmp list that holds all the running campaigns
	ets:insert(cmp_list, {Cmp, AccId, self(), Tid, 1.0}),
	State = #state{
		cmp = Cmp,
		tid = Tid,
		timestamp = erlang:timestamp(),
		hash = CmpHash
	},
	?INFO("BIDDER_CMP: Campaign ID [~p] initiated on node ~p", [Cmp, node()]),
	erlang:send_after(?INTERVAL, self(), {interval}),
	{ok, State}.


%%
%% Retrieve counts for previous t, to return them as resp
%%
handle_call({get_stats}, _From, State) ->
	Tid = State#state.tid,
	Resp = #{
		<<"bid_requests">> => try_ets_lookup_stats(Tid, <<"bid_requests">>),
		<<"bids">> => try_ets_lookup_stats(Tid, <<"bids">>),
		<<"imps">> => try_ets_lookup_stats(Tid, <<"imps">>),
		<<"clicks">> => try_ets_lookup_stats(Tid, <<"clicks">>)
	},
	{reply, {ok, Resp}, State};


handle_call({get_cmp_hash}, _From, State) ->
	Hash = State#state.hash,
	{reply, {ok, Hash}, State};

handle_call({reset_timeout}, _From, State) ->
	{reply, {ok, ok}, State#state{timestamp = erlang:timestamp()}};

handle_call({set_pacing_rate, Rate}, _From, State) ->
	Tid = State#state.tid,
	Resp = case ets:insert(Tid, {<<"pacing_rate">>, Rate}) of
			   true ->
				   {ok, ok};
			   _ ->
				   {error, ets_insert_error}
		   end,
	{reply, Resp, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


%%
%% Retrieve counts for previous t, to return them as resp.
%% At the same time, reset the counters in ETS of that previous t and save the
%% 	stats in State2
%%
handle_info({interval}, State) ->
	Tid = State#state.tid,
	Stats = #{
		<<"bid_requests">> => try_ets_lookup_stats(Tid, <<"bid_requests">>),
		<<"bids">> => try_ets_lookup_stats(Tid, <<"bids">>),
		<<"imps">> => try_ets_lookup_stats(Tid, <<"imps">>),
		<<"clicks">> => try_ets_lookup_stats(Tid, <<"clicks">>)
	},
	Cmp = State#state.cmp,
	Node = node(),
	PublishStats = #{
		<<"cmp">> => Cmp,
		<<"node">> => Node,
		<<"stats">> => Stats
	},
	rmq:publish(stats, erlang:term_to_binary(PublishStats)),
	%% check for timeout if didn't receive campaign update in CMP_TIMEOUT or
	%%	didn't receive pacing rate update in CMP_PACING_RATE_TIMEOUR
	T1 = State#state.timestamp,
	T2 = erlang:timestamp(),
	Tdiff = tk_lib:tdiff_seconds(T1, T2),
	case Tdiff > ?CMP_TIMEOUT orelse Tdiff > ?CMP_PACING_RATE_TIMEOUT of
		false ->
			ok;
		true ->
			?WARN("BIDDER_CMP: Campaign didn't receive pacing_rate or campaign update (Cmp: ~p initiated on node ~p)",
				[Cmp, node()]),
			self() ! {stop}
	end,
	%% Send to statsd and reset all ETS counters
	bidder_stats:send_stats(Tid),
	bidder_stats:reset_stats(Tid),
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State};

handle_info({stop}, State) ->
	{stop, shutdown, State};

handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

%%
%% @private
%% Stops the process gracefully, and cleans up all the ETS tables
%%
terminate(shutdown, State) ->
	Cmp = State#state.cmp,
	Tid = State#state.tid,
	ets:delete(cmp_list, Cmp),
	ets:delete(Tid),
	?WARN("BIDDER_CMP: Campaign ID [~p] is stopped on node ~p", [Cmp, node()]);
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

try_ets_lookup_stats(Table, StatKey) ->
	try_ets_lookup_stats(Table, StatKey, 0).
try_ets_lookup_stats(Table, StatKey, Default) ->
	case ets:lookup(Table, StatKey) of
		[{_, Val} | _] -> Val;
		[] -> Default
	end.

%% @hidden
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[Val | _] -> Val;
		[] -> Default
	end.