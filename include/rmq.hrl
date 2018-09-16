
%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

-record(subscriber, {
	name,
	exchange,
	type,
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



%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = cmp_config,
		exchange = <<"campaigns">>,
		type = pubsub,
		topic = <<"config.general">>,
		logging = false,
		func = fun(P) -> bidder_cmp:load_cmp_config(P) end,
		pool_size = 50},
	#subscriber{
		name = cmp_pacing,
		exchange = <<"campaigns">>,
		type = pubsub,
		topic = <<"config.pacing">>,
		logging = false,
		func = fun(P) -> bidder_cmp:set_pacing_rate(P) end,
		pool_size = 100},
	#subscriber{
		name = config_bert,
		exchange = <<"config">>,
		type = pubsub,
		topic = <<"bert">>,
		logging = true,
		func = fun(P) -> bidder_cmp:save_bert_file(P) end,
		pool_size = 5},
	#subscriber{
		name = wins,
		exchange = <<"wins">>,
		type = pubsub,
		topic = <<"wins.{id}">>,
		logging = true,
		func = fun(P) -> bidder_wins:mark_wins(P) end,
		pool_size = 20},
	#subscriber{
		name = imps,
		exchange = <<"imps">>,
		type = pubsub,
		topic = <<"imps.{id}">>,
		logging = false,
		func = fun(P) -> bidder_wins:mark_imps(P) end,
		pool_size = 20},
	#subscriber{
		name = clicks,
		exchange = <<"clicks">>,
		type = pubsub,
		topic = <<"clicks.{id}">>,
		logging = false,
		func = fun(P) -> bidder_wins:mark_click(P) end,
		pool_size = 5}

]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = bids_debug,
		exchange = <<"bids">>,
		logging = false,
		topic = <<"bids.debug">>,
		pool_size = 30}
]).
