%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author halid
%%% @copyright (C) 2016, tapklik
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(tkb).

-include("global.hrl").
-include("lager.hrl").


-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start() ->
	lists:foreach(
		fun(_X) ->
			supervisor:start_child(tkb_interface_sup, [])
		end
	, lists:seq(1, ?NUM_OF_INTERFACE_PROCESSES)).

stop() ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
