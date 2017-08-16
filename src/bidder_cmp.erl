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
	bids = 0,
	imps = 0,
	clicks = 0
}).

-define(CMP_TIMEOUT_MS, ?CMP_TIMEOUT * 1000).


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
		{_Cmp, Pid, _Tid, _Rate} ->
			case gen_server:call(Pid, {get_cmp_hash}) of
				{ok, Hash} ->
					gen_server:cast(Pid, {reset_timeout});
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
		{_, Pid, _, _} ->
			gen_server:cast(Pid, {reset_timeout}),
			{ok, already_started}
	end.


dirty_read_cmp(CmpTid, Key) ->
	try_ets_lookup(CmpTid, Key, not_found).

stop_cmp(Cmp) ->
	case try_ets_lookup(cmp_list, Cmp) of
		not_found ->
			{error, not_found};
		{_, Pid, _, _} ->
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
	case check_cmp(Cmp) of
		<<"not_found">> ->
			{ok, #{
				<<"cmp">> => Cmp,
				<<"status">> => <<"not_found">>
			}};
		{_Cmp, Pid, _Tid, _Rate} ->
			gen_server:call(Pid, {get_stats})
	end.

get_all_cmps() ->
	[Cmp || {Cmp, _, _, _} <- ets:tab2list(cmp_list)].

get_and_reset_all_cmps_stats() ->
	CmpPids = [Pid || {_, Pid, _, _} <- ets:tab2list(cmp_list)],
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
	case check_cmp(Cmp) of
		<<"not_found">> ->
			ok;
		{_Cmp, Pid, _Tid, _Rate} ->
			gen_server:call(Pid, {set_pacing_rate, Rate})
	end.

save_bert_file(FileBin) ->
	[{Filename1, _Content} | _] = binary_to_term(FileBin),
	Filename2 = atom_to_list(Filename1),
	case file:write_file(?DATA_PATH ++ Filename2 ++ ".bert", FileBin) of
		ok ->
			?INFO("RMQ: File ~p is received and saved", [Filename2]);
		{error, E} ->
			?ERROR("RMQ: Error in receiving or saving file ~p. (Error: ~p) ", [Filename2, E])
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
		<<"id">> := Cid
	} = CmpConfig,
	ets:insert(Tid, [
		{<<"filters">>, Filters},
		{<<"creatives">>, Creatives},
		{<<"status">>, Status},
		{<<"cid">>, Cid},
		{<<"hash">>, CmpHash},
		{<<"pacing_rate">>, 1.0}
	]),
	ets:insert(Tid, [
		{<<"bids">>, 0},
		{<<"impressions">>, 0},
		{<<"clicks">>, 0}
	]),
	%% Global cmp list that holds all the running campaigns
	ets:insert(cmp_list, {Cmp, self(), Tid, 1.0}),
	State = #state{
		cmp = Cmp,
		tid = Tid,
		timestamp = erlang:timestamp(),
		hash = CmpHash
	},
	?INFO("BIDDER_CMP: Campaign ID [~p] initiated on node ~p", [Cmp, node()]),
	erlang:send_after(?CMP_TIMEOUT_MS, self(), {timeout}),
	{ok, State}.


%%
%% Retrieve counts for previous t, to return them as resp
%%
handle_call({get_stats}, _From, State) ->
	Resp = #{
		<<"bids">> => State#state.bids,
		<<"impressions">> => State#state.imps,
		<<"clicks">> => State#state.clicks
	},
	{reply, {ok, Resp}, State};

%%
%% Retrieve counts for previous t, to return them as resp.
%% At the same time, reset the counters in ETS of that previous t and save the
%% 	stats in State2
%%
handle_call({get_and_reset_stats}, _From, State) ->
	Tid = State#state.tid,
	Resp = #{
		<<"cmp">> => State#state.cmp,
		<<"bids">> => State#state.bids,
		<<"impressions">> => State#state.imps,
		<<"clicks">> => State#state.clicks
	},
	%% Store current counts from the ETS
	[{_, Bids}] = ets:lookup(Tid, <<"bids">>),
	[{_, Imps}] = ets:lookup(Tid, <<"impressions">>),
	[{_, Clicks}] = ets:lookup(Tid, <<"clicks">>),
	State2 = State#state{
		bids = Bids,
		imps = Imps,
		clicks = Clicks
	},
	%% Reset all ETS counters
	ets:insert(Tid, {<<"bids">>, 0}),
	ets:insert(Tid, {<<"impressions">>, 0}),
	ets:insert(Tid, {<<"clicks">>, 0}),
	{reply, {ok, Resp}, State2};

handle_call({get_cmp_hash}, _From, State) ->
	Hash = State#state.hash,
	{reply, {ok, Hash}, State};

handle_call({set_pacing_rate, Rate}, _From, State) ->
	Tid = State#state.tid,
	ets:insert(Tid, {<<"pacing_rate">>, Rate}),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({reset_timeout}, State) ->
	{noreply, State#state{timestamp = erlang:timestamp()}};
handle_cast(_Request, State) ->
	{noreply, State}.

%%
%% @private
%% Messages that are covered through handle_info() include:
%%
%% 1) {timeout}: periodic check to see when was the last update received; if it
%% 		is greater than the defined ?CMP_TIMEOUT the campaign is halted
%%
%% 2) {stop}: stops this gen_server through normal shutdown
%%
handle_info({timeout}, State) ->
	T1 = State#state.timestamp,
	T2 = erlang:timestamp(),
	Tdiff = tk_lib:tdiff_seconds(T1, T2),
	case Tdiff > ?CMP_TIMEOUT of
		false ->
			erlang:send_after(?CMP_TIMEOUT_MS, self(), {timeout});
		true ->
			self() ! {stop}
	end,
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


%% @hidden
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[Val | _] -> Val;
		[] -> Default
	end.