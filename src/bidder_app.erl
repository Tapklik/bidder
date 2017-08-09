-module(bidder_app).

-behaviour(application).

-include("bidder_global.hrl").
-include("lager.hrl").
-include("rmq.hrl").

-export([start/0, start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start() ->
	_ = [application:start(Dep) || Dep <- resolve_deps(bidder),
		not is_otp_base_app(Dep)],
	%% Start RMQ Pub/Sub workers
	[rmq:start_subscriber(Subscriber) || Subscriber <- ?RMQ_SUBSCRIBERS],
	[rmq:start_publisher(Publisher) || Publisher <- ?RMQ_PUBLISHERS],
	tkb:start().

start(_Type, _Args) ->
	bidder_sup:start_link().

stop(_State) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%


dep_apps(App) ->
	application:load(App),
	{ok, Apps} = application:get_key(App, applications),
	Apps.

all_deps(App, Deps) ->
	[[all_deps(Dep, [App | Deps]) || Dep <- dep_apps(App),
		not lists:member(Dep, Deps)], App].

resolve_deps(App) ->
	DepList = all_deps(App, []),
	{AppOrder, _} = lists:foldl(fun(A, {List, Set}) ->
		case sets:is_element(A, Set) of
			true ->
				{List, Set};
			false ->
				{List ++ [A], sets:add_element(A, Set)}
		end
								end,
		{[], sets:new()},
		lists:flatten(DepList)),
	AppOrder.

is_otp_base_app(kernel) ->
	true;
is_otp_base_app(stdlib) ->
	true;
is_otp_base_app(_) ->
	false.
