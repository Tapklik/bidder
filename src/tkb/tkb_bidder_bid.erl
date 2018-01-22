-module(tkb_bidder_bid).

-export([calc_bid/3]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

calc_bid(<<"random">>, _, BidFloor) ->
	case rand:uniform(100) / 10 of
		B when B >= BidFloor -> B;
		_ -> no_bid
	end;
calc_bid(<<"exact">>, Bid, BidFloor) ->
	case round_decimal(abs(Bid / 1000000)) of
		B when B >= BidFloor -> B;
		_ -> no_bid
	end;
calc_bid(<<"variance">>, Bid, BidFloor) ->
	case round_decimal(abs(Bid / 1000000) * (110 - rand:uniform(40)) / 100) of
		B when B >= BidFloor -> B;
		_ -> no_bid
	end.


%%%%%%%%%%%%%%%%%%%%%%
%%%    PRIVATE 	   %%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

round_decimal(X) ->
	trunc(X * 100) / 100.