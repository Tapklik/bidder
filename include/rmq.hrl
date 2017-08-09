%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

-record(subscriber, {
	name,
	exchange,
	topic,
	func,
	pool_size
}).

-record(publisher, {
	name,
	exchange,
	topic,
	pool_size
}).

%% RABBITMQ PUBSUB SETTINGS
-define(RMQ_HOST, "de-c1.srv.boss01").
-define(RMQ_PORT, 5672).
-define(RMQ_USER, <<"tapklik">>).
-define(RMQ_PASSWORD, <<"tapKlik7-rabbitmq">>).
-define(RMQ_VHOST, <<"/erl">>).


%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = cmp_config,
		exchange = <<"campaigns">>,
		topic = <<"config.general">>,
		func = fun(P) -> bidder_cmp:load_config(P) end,
		pool_size = 5},
	#subscriber{
		name = config_bert,
		exchange = <<"config">>,
		topic = <<"bert">>,
		func = fun(P) -> bidder_cmp:save_bert_file(P) end,
		pool_size = 5},
	#subscriber{
		name = wins,
		exchange = <<"wins">>,
		topic = <<"wins.imps">>,
		func = fun(P) -> bidder_data:mark_win(P) end,
		pool_size = 10},
	#subscriber{
		name = clicks,
		exchange = <<"wins">>,
		topic = <<"wins.clicks">>,
		func = fun(P) -> bidder_data:mark_click(P) end,
		pool_size = 4}
]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = cmp_stats,
		exchange = <<"campaigns">>,
		topic = <<"stats.bidders">>,
		pool_size = 5},
	#publisher{
		name = bids_debug,
		exchange = <<"bids">>,
		topic = <<"bids.debug">>,
		pool_size = 30}
]).
