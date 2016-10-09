-module(logger).
-include("irc.hrl").
-export([write/2,
	 generate_timestamp/0,
	 generate_date_timestamp/0,
	 generate_datestamp/0,
	 time_with_two_digits/1
	]).

write(Module, Message) ->
    Format = "[~s] ~s: ~s",
    Output_message = io_lib:format(Format, [?TIMESTAMP, Module, Message]),
    file:write_file("./ircer.log", Output_message, [append]).

generate_date_timestamp() ->
    Time_text = generate_timestamp(),
    Date_text = generate_datestamp(),
    Date_text ++ " " ++ Time_text.

generate_datestamp() ->
    {{Year, Month, Day}, _} = calendar:local_time(),
    integer_to_list(Year) ++ "-" ++
	time_with_two_digits(Month) ++ "-" ++
	time_with_two_digits(Day).

generate_timestamp() ->
    {_, {Hour, Minute, Second}} = calendar:local_time(),
    time_with_two_digits(Hour) ++ ":" ++
	time_with_two_digits(Minute) ++ ":" ++
	time_with_two_digits(Second).

time_with_two_digits(Any_time_unit) ->
    lists:flatten(io_lib:format("~2..0B", [Any_time_unit])).
