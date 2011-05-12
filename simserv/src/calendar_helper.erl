-module(calendar_helper).
-export([time_in_words/1,yesterday/1]).

-define(SECONDS_IN_A_DAY,86400).

day_in_words(Day) ->
	case Day of
	1 -> "Mon";
	2 -> "Tue";
	3 -> "Wed";
	4 -> "Thur";
	5 -> "Fri";
	6 -> "Sat";
	7 -> "Sun"
	end.

month_in_words(Month) ->
	case Month of
	1 -> "Jan";
	2 -> "Feb";
	3 -> "Mar";
	4 -> "Apr";
	5 -> "May";
	6 -> "Jun";
	7 -> "Jul";
	8 -> "Aug";
	9 -> "Sep";
	10 -> "Oct";
	11 -> "Nov";
	12 -> "Dec"
	end.

yesterday({Year,Month,Day}) ->
	TodaySec = calendar:datetime_to_gregorian_seconds({Year,Month,Day},{0,0,0}),
	YesterdaySec = TodaySec - ?SECONDS_IN_A_DAY,
	element(1,calendar:gregorian_seconds_to_datetime(YesterdaySec)).

prepend_zero(D) when D < 10 -> lists:flatten(io_lib:format("0~.10B",[D]));
prepend_zero(D) -> lists:flatten(io_lib:format("~.10B",[D])).

time_in_words(Date) ->
	{{Year,Month,Day},{Hour,Minute,Second}} = Date,
	StrDay = day_in_words(calendar:day_of_the_week({Year,Month,Day})),
	StrMonth = month_in_words(Month),
	lists:flatten(io_lib:format("~s, ~s ~s ~p ~s:~s:~s GMT",[StrDay,prepend_zero(Day),StrMonth,Year,prepend_zero(Hour),prepend_zero(Minute),prepend_zero(Second)])).
