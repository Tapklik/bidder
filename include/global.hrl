%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

%% SYSTEM SETTINGS
-define(MODEL_TIMEOUT, 40).
-define(AUCTION_TIMEOUT, 70).
-define(STATS_P, 0.3).
-define(FILTERING, true).
-define(NODE, node()).


-define(DATA_PATH, "./data/"). %% Add the trailing "/"
-define(BIDS_DATA_PATH, "./data/bids/"). %% Add the trailing "/"

-define(APPLICATION, bidder).
-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).


-define(CMP_TIMEOUT, 300). %% seconds
-define(CMP_PACING_RATE_TIMEOUT, 30). %% seconds

-define(BIDS_SAVE_PCTG, 0.2). %% should be less than 1

-define(RMQ_HOST, ?ENV(rmq_host, "localhost")).
-define(BIDS_STREAM_TOPIC, ?ENV(stream_bids_topic, <<"Bids">>)).
-define(MODEL_PORT, ?ENV(model_port, 65327)).



%% DEFAULTS


%% COWBOY SETTINGS
-define(COWBOY_GW_ACCEPTORS, 1500).
-define(COWBOY_GW_PORT, 2234).


%% tkb config
%% ===========================================
%% INTERFACE SETTINGS
-define(NUM_OF_INTERFACE_PROCESSES, 10).
%% POOLER SETTINGS (per interface process)
-define(BIDS_PLUS_POOLER_INIT_COUNT, 100).
-define(BIDS_PLUS_POOLER_MAX_COUNT, 200).



-ifdef(debug).
-warning("Debug mode is ON!").
-define(DEBUG_BIDS_RATE, 1.0).
-else.
-define(DEBUG_BIDS_RATE, 0.0).
-endif.







%%%%%%%%%%%%%%%%%%%%%%
%%%    RABBITMQ    %%%
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

-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = cmp_config,
		exchange = <<"campaigns">>,
		type = pubsub,
		topic = <<"config.general">>,
		logging = false,
		func = fun(P) -> bidder_cmp:load_cmp_config(P) end,
		pool_size = 10},
	#subscriber{
		name = cmp_pacing,
		exchange = <<"campaigns">>,
		type = pubsub,
		topic = <<"config.pacing">>,
		logging = false,
		func = fun(P) -> bidder_cmp:set_pacing_rate(P) end,
		pool_size = 30},
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
		topic = [<<"wins">>, ?ENV(app_id, <<"*">>)],
		logging = false,
		func = fun(P) -> bidder_wins:mark_wins(P) end,
		pool_size = 20},
	#subscriber{
		name = imps,
		exchange = <<"imps">>,
		type = pubsub,
		topic = [<<"imps">>, ?ENV(app_id, <<"*">>)],
		logging = false,
		func = fun(P) -> bidder_wins:mark_imps(P) end,
		pool_size = 20},
	#subscriber{
		name = clicks,
		exchange = <<"clicks">>,
		type = pubsub,
		topic = [<<"clicks">>, ?ENV(app_id, <<"*">>)],
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

