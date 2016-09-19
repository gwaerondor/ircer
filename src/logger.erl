-module(logger).
-export([write/2,
	 generate_timestamp/0,
	 generate_date_timestamp/0,
	 generate_datestamp/0,
	 time_with_two_digits/1
	]).

write(Module, Message) ->
    Timestamp = generate_timestamp(),
    Message_to_write = "["++Timestamp++"] " ++ Module ++ ": " ++ Message ++ "\n",
    file:write_file("./ircer.log", Message_to_write,[append]).

generate_date_timestamp() ->
    Time_text = generate_timestamp(),
    Date_text = generate_datestamp(),
    Date_text ++ " " ++ Time_text.

generate_datestamp() ->
    {Date, _} = calendar:local_time(),
    {Year_now, Month_now, Day_now} = Date,
    integer_to_list(Year_now) ++ "-" ++
	time_with_two_digits(Month_now) ++ "-" ++
	time_with_two_digits(Day_now).

generate_timestamp() ->
    {_, Time} = calendar:local_time(),
    {Hour_now, Minute_now, Second_now} = Time,
    time_with_two_digits(Hour_now) ++ ":" ++
	time_with_two_digits(Minute_now) ++ ":" ++
	time_with_two_digits(Second_now).
    

time_with_two_digits(Any_time_unit) ->
    case Any_time_unit < 10 of
	true ->
	    "0" ++ integer_to_list(Any_time_unit);
	false ->
	    integer_to_list(Any_time_unit)
    end.
