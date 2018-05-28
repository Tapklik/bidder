
%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

-record(subscriber, {
	name,
	exchange,
	topic,
	func,
	pool_size,
	logging
}).

-record(publisher, {
	name,
	exchange,
	topic,
	pool_size,
	logging
}).

%% RABBITMQ PUBSUB SETTINGS
% Define RMQ_HOST in global.hrl
-define(RMQ_PORT, 5672).
-define(RMQ_USER, <<"tapklik">>).
-define(RMQ_PASSWORD, <<"tapKlik7-rabbitmq">>).
-define(RMQ_VHOST, <<"/erl">>).

-define(RMQ_X_MESSAGE_TTL, 60).


%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = cmp_config,
		exchange = <<"campaigns">>,
		topic = <<"config.general">>,
		logging = true,
		func = fun(P) -> bidder_cmp:load_cmp_config(P) end,
		pool_size = 50},
	#subscriber{
		name = cmp_pacing,
		exchange = <<"campaigns">>,
		topic = <<"config.pacing">>,
		logging = true,
		func = fun(P) -> bidder_cmp:set_pacing_rate(P) end,
		pool_size = 100},
	#subscriber{
		name = config_bert,
		exchange = <<"config">>,
		topic = <<"bert">>,
		logging = true,
		func = fun(P) -> bidder_cmp:save_bert_file(P) end,
		pool_size = 5},
	#subscriber{
		name = wins,
		exchange = <<"wins">>,
		topic = <<"wins.{id}">>,
		logging = false,
		func = fun(P) -> bidder_wins:mark_win(P) end,
		pool_size = 100}

]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = bids_debug,
		exchange = <<"bids">>,
		logging = false,
		topic = <<"bids.debug">>,
		pool_size = 30}
]).
