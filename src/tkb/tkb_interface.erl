-module(tkb_interface).

-behaviour(gen_server).

-include("bidder_global.hrl").
-include("lager.hrl").


-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).
-export([start_bids_pool/1, stop_bids_pool/1]).



-record(state, {
	name,
	s_group = {}
}).

-define(TIMEOUT, 10000).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link(?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	Name = uid(),
	start_bids_pool(Name),
	erlang:send_after(?TIMEOUT, self(), check_s_group),
	{ok, #state{name = Name}}.

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(check_s_group, State) ->
	case find_bidders_s_group() of
		{} ->
			?WARN("BIDDER: Interface GS at ~p is not a part of any bidders s_group.", [node()]),
			State2 = State;
		S_Group ->
			Name = State#state.name,
			case register_to_s_group(S_Group, {bidder, Name}) of
				{no, name_or_pid_already_registered} ->
					State2 = State#state{s_group = S_Group};
				{no, E} ->
					State2 = State#state{s_group = {}},
					?ERROR("BIDDER: Interface GS [Name: ~p (pid: ~p)] couldn't register to "
					"s_group ~p. (Error: ~p).", [Name, self(), S_Group, E]);
				_ ->
					State2 = State#state{s_group = S_Group},
					?INFO("BIDDER: Interface GS [Name: ~p (pid: ~p)] successfully registered "
					"and declared bidder worker to s_group ~p.", [Name, self(), S_Group])
			end
	end,
	erlang:send_after(?TIMEOUT, self(), check_s_group),
	{noreply, State2};

handle_info({_BRfrom, br, _BidId, _BRplus, _TimeStamp, _DebugBid} = BR, State) when State#state.s_group =/= {} ->
	PoolName = State#state.name,
	Worker = pooler:take_member(PoolName),
	Worker ! {BR, PoolName},
	{noreply, State};


handle_info(stop, State) ->
	{stop, normal, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State};
handle_info(_BR, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

start_bids_pool(Name) ->
	PoolConfig1 = [
		{name, Name},
		{max_count, ?BIDS_PLUS_POOLER_MAX_COUNT}, {init_count, ?BIDS_PLUS_POOLER_INIT_COUNT},
		{start_mfa, {tkb_bids_handler, start_link, []}}
	],
	pooler:new_pool(PoolConfig1).

stop_bids_pool(Name) ->
	pooler:rm_pool(Name).

find_bidders_s_group() ->
	S_Groups = [{bidders, SG} || {{bidders, SG}, _} <- s_group:own_s_groups()],
	case S_Groups of
		[] -> {};
		[H | _] -> H
	end.

register_to_s_group(S_Group, Name) ->
	s_group:register_name(S_Group, {bidder, Name}, self()).


uid() ->
	Chrs = list_to_tuple("abcdefghijklmnopqrstuvwxyz0123456789"),
	ChrsSize = size(Chrs),
	F = fun(_, R) -> [element(rand:uniform(ChrsSize), Chrs) | R] end,
	list_to_atom("b_" ++ lists:foldl(F, "", lists:seq(1, 8))).