-module(printer).
-export([print_incoming_server_message/1,
	 print_outgoing_server_message/1,
	 print_warning_message/1,
	 print_error_message/1,
	 print_custom/1,
	 print_regular_message/1
	]).

print_incoming_server_message(Message) ->
    io:format("> ~s~n", [Message]).

print_outgoing_server_message(Message) ->
    io:format("< ~s~n", [Message]).

print_warning_message(Message) ->
    io:format("/!\\ ~s~n", [Message]).

print_error_message(Message) ->
    io:format("!!! ~s~n", [Message]).

print_custom(Message) ->
    io:format("~s~n", [Message]). 

print_regular_message(Message) ->
    io:format("~s~n", [Message]).
