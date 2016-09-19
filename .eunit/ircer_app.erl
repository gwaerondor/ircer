-module(ircer_app).
-behaviour(application).
-include("irc.hrl").

%% Application callbacks
-export([
	 start/2,
	 start/4,
	 start/0,
	 stop/1,
	 handle_messages/1
	]).

-define(print_warning(Msg), printer:print_warning_message(Msg)).
-define(print_err(Msg), printer:print_error_message(Msg)).
-define(print_inc(Msg), printer:print_incoming_server_message(Msg)).
-define(print_out(Msg), printer:print_outgoing_server_message(Msg)).
-define(print_msg(Msg), printer:print_regular_message(Msg)).
-define(log(Msg), logger:write("ircer_app", Msg)).
-define(GET_SOCKET,
	fun(#connection_data{socket=Sock}) -> Sock end).
-define(SET_SOCKET, fun(Sock, Connection_data) ->
        Connection_data#connection_data{socket=Sock} end).
-define(GET_SERVER_AND_PORT,
	fun(#connection_data{server=Server, port=Port}) -> [Server, Port] end).
-define(IGNORE_RESULTS,
	fun(_, Connection_data) -> Connection_data end).
-define(GET_NICK_AND_REALNAME,
	fun(#connection_data{nick=Nickname, realname=Realname}) ->
	[Nickname, Realname] end).
-define(GET_REALNAME,
	fun(#connection_data{realname=Realname}) ->
	Realname end).
-define(GET_NICK,
	fun(#connection_data{nick=Nickname}) ->
	Nickname end).
-define(NOTHING,
	fun(#connection_data{}) ->
	nothing end).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_, Start_args) ->
    ?log("=== Starting IRCER client ==="),
    [Server, Port, Nickname, Realname] = Start_args,
    connect(Server, Port, Nickname, Realname),
    {ok, self()}.

start(Server, Port, Nickname, Realname) ->
    Start_args = [Server, Port, Nickname, Realname],
    start(undefined, Start_args).

start() ->
    Start_args = ["port80b.se.quakenet.org", 6667, "Gwaeron", "Robin Larsson"],
    start(undefined, Start_args).

stop(_State) ->
    ok.

%% ===================================================================
%% Functions
%% ===================================================================

connect(Server, Port, Nickname, Realname) ->
    Connection_data = #connection_data{server = Server, 
				       port = Port,
				       nick = Nickname, 
				       realname = Realname
				      },
    ?log("Trying to connect to "++ Server ++ " as " ++ Nickname),
    connect_sequence([
		      open_connection(),
		      start_socket_handler(),
		      request_nick(),
		      send_ident()
		     ], Connection_data),
    ok.

connect_sequence([], _Connection_data)->
    ok;
connect_sequence([{F, Get, Set} | T], Connection_data)->
    connect_sequence(T, Set(F(Get(Connection_data)), Connection_data)).

open_connection() ->
    {fun open_connection/1,
     ?GET_SERVER_AND_PORT,
     ?SET_SOCKET
    }.

open_connection([Server, Port]) ->
    open_connection(Server, Port).

open_connection(Server, Port) ->
    case gen_tcp:connect(Server,Port,[{active, false}]) of
	{ok, Sock} ->
	    ?log("Connected. Now logging in."),
	    Sock;
	Err ->
	    ?log("Could not connect to server " ++ Server ++ ":" ++ integer_to_list(Port)),
	    Err
    end.

request_nick() ->
    {fun request_nick/1,
     ?GET_NICK,
     ?IGNORE_RESULTS
    }.

request_nick(Nickname) ->
    i:nick(Nickname).

send_ident() ->
    {fun send_ident/1,
     ?GET_NICK_AND_REALNAME,
     ?IGNORE_RESULTS
    }.

start_socket_handler(Sock) ->
    socket_handler:start(Sock).

start_socket_handler() ->
    { fun start_socket_handler/1,
      ?GET_SOCKET,
      ?IGNORE_RESULTS
    }.

send_ident([Nickname, Realname]) ->
    i:send_ident(Nickname, Realname).

handle_messages([Message|More_messages]) ->
    Decoded_message = codec:decode_message(Message),
    case Decoded_message#message.type of
	ping ->
	    ?log("Ping " ++ Decoded_message#message.text),
	    pong(Decoded_message#message.text);
	_ ->
	    ?print_inc(codec:make_printable(Decoded_message))
    end,
    handle_messages(More_messages);
handle_messages([]) ->
    ok.

pong(Number) ->
    Outgoing = #message{type=pong, text=Number},
    Outgoing_enc = codec:encode_message(Outgoing),
    socket_handler ! {message, Outgoing_enc},
    ?log("Pong " ++ Number).

