-module(bidrequest_helper).

-compile(export_all).


%% BID REQUESTS
bid_request_1() ->
	#{<<"badv">> => [<<"bmw.com">>],
		<<"bcat">> => [<<"IAB24">>],
		<<"cat">> => [<<"IAB1">>, <<"IAB1-6">>],
		<<"device">> => {4, <<>>, <<"Apple">>, <<"iOS">>, <<"Mozilla/5.0">>},
		<<"geo">> => {<<"SAU">>, <<"01">>, <<"Riyadh">>, <<"Riyadh">>},
		<<"id">> => <<"BR-73499275800816">>,
		<<"imp">> => #{<<"bidfloor">> => 2.35,
			<<"blocked">> => {[1, 3], []},
			<<"dim">> => [1052, 1053],
			<<"expdir">> => 0,
			<<"pos">> => 1,
			<<"type">> => <<"banner">>},
		<<"ip">> => <<"95.246.223.14">>,
		<<"language">> => <<"en">>,
		<<"site">> => #{<<"cat">> => [<<"IAB1">>, <<"IAB1-6">>],
			<<"domain">> => <<"www.sm3na.com">>,
			<<"id">> => <<"WW-47262948763574">>},
		<<"user">> => {<<"USR-79040041481540">>, 21}}.
