-module(logger_tests).

-ifdef(TEST).
-include("irc.hrl").
-include_lib("eunit/include/eunit.hrl").

generate_date_timestamp_test() ->
    Result = logger:generate_date_timestamp(),
    {Date, Time} = calendar:local_time(),
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    Year_t = integer_to_list(Year),
    Month_t = logger:time_with_two_digits(Month),
    Day_t = logger:time_with_two_digits(Day),
    Hour_t = logger:time_with_two_digits(Hour),
    Minute_t = logger:time_with_two_digits(Minute),
    Second_t = logger:time_with_two_digits(Second),
    Expected = Year_t++"-"++Month_t++"-"++Day_t++
	" "++Hour_t++":"++Minute_t++":"++Second_t,
    ?assertEqual(Expected, Result).

-endif.
