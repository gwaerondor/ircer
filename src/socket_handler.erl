-module(socket_handler).
-include("irc.hrl").
-export([start/1,
	 receive_messages/1]).

-define(print_warning(Msg), printer:print_warning_message(Msg)).
-define(print_err(Msg), printer:print_error_message(Msg)).
-define(print_inc(Msg), printer:print_incoming_server_message(Msg)).
-define(print_out(Msg), printer:print_outgoing_server_message(Msg)).
-define(print_msg(Msg), printer:print_regular_message(Msg)).
-define(log(Msg), logger:write("socket_handler", Msg)).

start(Sock) ->
    spawn(fun() ->
		  register(?MODULE, self()),
		  handle_messages_until_exit(Sock) end
	 ),
    spawn(?MODULE, receive_messages, [Sock]).

receive_messages(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Msg} ->	
	    ?MODULE ! {incoming, Sock, Msg},
	    receive_messages(Sock);
	{error, closed} ->
	    ?log("Connection has been closed."),
	    ?log("Could not receive messages, socket has been closed."),
	    ok;
	Error ->
	    ErrorStr = io_lib:format("~p",[Error]),
	    ?MODULE ! quit,
	    Error_message_receiver = "Error in message receiver: " ++ ErrorStr,
	    Error_message_closing = "Message receiver is shutting down. "
                                    "Your connection has been closed.",
	    ?log(Error_message_receiver),
	    ?log(Error_message_closing),
	    ?print_err(Error_message_receiver),
	    ?print_err(Error_message_closing)
    end.

handle_messages_until_exit(Sock) ->
    handle_messages_until_exit(Sock, "").

handle_messages_until_exit(Sock, Message_rest) ->
    receive
	{outgoing, Outgoing_message} ->
	    send_to_server(Sock, Outgoing_message),
	    handle_messages_until_exit(Sock, Message_rest);
	{incoming, Sock, Incoming_msg} ->
	    {Msgs, Unfinished_msg} = codec:parse_lines(Message_rest ++ Incoming_msg),
	    ircer_app:handle_messages(Msgs),
	    handle_messages_until_exit(Sock, Unfinished_msg);
	quit ->
	    gen_tcp:shutdown(Sock, read_write),
	    ?print_warning("Connection has been closed.")
    end.

send_to_server(Sock, Message) ->
    case gen_tcp:send(Sock, Message ++ "\r\n") of
	ok ->
	    ok;
	{error, Reason} ->
	    Error_message = io_lib:format("Could not send to server: ~p",[Reason]),
	    ?log(Error_message),
	    {error, Reason}
    end.
