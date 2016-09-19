-module(i).
-include("irc.hrl").
-export([
	 msg/2,
	 join/1,
	 quit/1,
	 quit/0,
	 part/2,
	 nick/1,
	 part/1,
	 connect/3,
	 connect/4,
	 send_ident/2
	]).
-define(log(Msg), logger:write("i.erl", Msg)).	       

send_ident(Nickname, Realname) ->
    Ident = #message{type = ident,
		     sender = Nickname,
		     text = Realname
		    },
    socket_handler ! {message, codec:encode_message(Ident)}.

% This is where the user commands live.
msg(Target, Message) ->
    Outgoing = #message{type=privmsg, receiver=Target, text=Message},
    Outgoing_enc = codec:encode_message(Outgoing),
    socket_handler ! {message, Outgoing_enc}.

join(Channel) ->
    Outgoing = #message{type=join, channel=Channel},
    Outgoing_enc = codec:encode_message(Outgoing),
    ?log("Joining channel " ++ Channel),
    socket_handler ! {message, Outgoing_enc}.

nick(Nickname) ->
    Outgoing = #message{type = nick,
		       text = Nickname
		      },
    Outgoing_enc = codec:encode_message(Outgoing),
    ?log("Changing nickname to " ++ Nickname),
    socket_handler ! {message, Outgoing_enc}.

part(Channel) ->
    part(Channel, "").

part(Channel, Message) ->
    Outgoing = #message{type = part,
			channel = Channel,
			text = Message},
    Outgoing_enc = codec:encode_message(Outgoing),
    ?log("Parted channel " ++ Channel),
    socket_handler ! {message, Outgoing_enc}.

quit(Message) ->
    % How do I check if a node/process is active?
    % I want to skip doing this if it is inactive!
    Outgoing = #message{type = quit,
			text = Message},
    Outgoing_enc = codec:encode_message(Outgoing),
    ?log("Quitting IRCer."),
    socket_handler ! {message, Outgoing_enc},
    socket_handler ! quit.

quit() ->
    quit("I was using IRCer v. 874442773.01").

connect(Server, Port, Nickname, Realname) ->
    quit(),
    ircer_app:start(Server, Port, Nickname, Realname).

connect(Server, Port, Nickname) ->
    connect(Server, Port, Nickname, Nickname).
