-module(tkb_bidder_model).

-include("global.hrl").
-include("lager.hrl").

-export([calc_bid/5]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

calc_bid(<<"random">>, _, _, BidFloor, _Rate) ->
	case rand:uniform(100) / 10 of
		B when B >= BidFloor -> B;
		_ -> {no_bid, bidfloor}
	end;
calc_bid(<<"exact">>, _ModelBR, Bid, BidFloor, _Rate) ->
	case floor_decimal(abs(Bid / 1000000), 2) of
		B when B >= BidFloor -> B;
		_ -> {no_bid, bidfloor}
	end;
calc_bid(<<"variance">> = Model, ModelBR, Bid, BidFloor, Rate) ->
	Rate2 = round_decimal(Rate, 1), tk_lib:echo1(rate, Rate),
	case ?ENV(model_enabled) andalso Rate2 =/= 0.0 of
		true ->
			case try_ets_lookup(models, {Model, Rate2}) of
				not_found ->
					?ERROR("BIDDER_MODEL: Error in model << ~p >>. (Error: No value in ETS or ETS not found!)", [Model]);
				{_, Threshold} -> tk_lib:echo1(model_br, ModelBR),
					bidder_model:get_prediction_async(ModelBR),
					receive
						{no_bid, Error} ->
							{no_bid, Error};
						{bid, Resp} -> tk_lib:echo1(ctr, Resp),
							#{
								<<"bid-id">> := _BidId,
								<<"probablity">> := Probablity
							} = Resp,
							case Probablity >= Threshold of
								true -> get_price_variance(Bid, Rate);
								_ -> {no_bid, bidfloor}
							end
					after ?MODEL_TIMEOUT ->
						{no_bid, model_timeout}
					end

			end;
		false when Rate == 0.0 ->
			{no_bid, pacing_rate};
		_ ->
			case floor_decimal(abs(Bid / 1000000) * (110 - rand:uniform(40)) / 100, 2) of
				B when B >= BidFloor -> B;
				_ -> {no_bid, bidfloor}
			end
	end.


%%%%%%%%%%%%%%%%%%%%%%
%%%    PRIVATE 	   %%%
%%%%%%%%%%%%%%%%%%%%%%

get_price_variance(Bid, Rate) when Rate >= 0.3->
	Bid;
get_price_variance(Bid, Rate) when Rate == 0.2->
	Bid * 1.2;
get_price_variance(Bid, Rate) when Rate == 0.1->
	Bid * 1.6.



%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%


floor_decimal(X, 1) ->
	trunc(X * 10) / 10;
floor_decimal(X, 2) ->
	trunc(X * 100) / 100;
floor_decimal(X, 3) ->
	trunc(X * 1000) / 1000.


round_decimal(X, 1) ->
	round(X * 10) / 10;
round_decimal(X, 2) ->
	round(X * 100) / 100;
round_decimal(X, 3) ->
	round(X * 1000) / 1000.


%% @hidden
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[Val | _] -> Val;
		[] -> Default
	end.


