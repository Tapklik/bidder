%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

%% SYSTEM SETTINGS
-define(AUCTION_TIMEOUT, 30).
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
-define(STREAM_BIDS_TOPIC, ?ENV(bids_stream_topic, <<"Bids">>)).



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
