-module(tk_time).


%% API
-export([get_current_time/0, get_current_time_slot/0, time_slot_end_time/0]).

-define(DAY, 24 * ?HOUR).
-define(HOUR, 60 * ?MIN).
-define(MIN, 60).
-define(TIME_ZONE, 4).
-define(TS, application:get_env(ts_length)).


get_current_time()->
	TZ = ?TIME_ZONE,
	Now = calendar:now_to_universal_time(erlang:timestamp()),
	time_shift(Now,TZ, hours).

get_current_time_slot()->
	D = ?DAY,
	Ts = ?TS,
	TotalSlots = trunc(D/Ts),
	Now = get_current_time(),
	Diff = time_diff(Now, day_end_time()),
	LeftSlots = trunc(Diff/Ts),
	{TotalSlots-LeftSlots, TotalSlots}.

time_slot_end_time()->
	Ts = ?TS,
	Now = get_current_time(),
	NowSecs = calendar:datetime_to_gregorian_seconds(Now),
	ThenSecs = (trunc(NowSecs / Ts) + 1) * Ts - 1,
	calendar:gregorian_seconds_to_datetime(ThenSecs).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

day_end_time()->
	Today = get_current_time(),
	{TomDate, _TomTime} = time_shift(Today,1,days),
	{TomDate, {00,00,00}}.

time_shift(Time, Value, days)->
	time_shift(Time, 24 * Value, hours);
time_shift(Time ,Value, hours)->
	time_shift(Time, 60 * Value, mins);
time_shift(Time, Value, mins)->
	time_shift(Time, 60 * Value, secs);
time_shift(Time, Value, secs)->
	NowSecs = calendar:datetime_to_gregorian_seconds(Time),
	NewTimeSecs = NowSecs + Value,
	calendar:gregorian_seconds_to_datetime(NewTimeSecs).

time_diff(Time1, Time2)->
	Sec1 = calendar:datetime_to_gregorian_seconds(Time1),
	Sec2 = calendar:datetime_to_gregorian_seconds(Time2),
	Sec2 - Sec1.