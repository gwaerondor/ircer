-module(codec).
-include("irc.hrl").
-export([decode_message/1,
	 parse_lines/1,
	 get_parameters/1,
	 get_command/1,
	 get_space_separated_tokens/2,
	 make_printable/1,
	 remove_leading_colon/1,
	 ensure_leading_pound/1,
	 text_before_exclamation_mark/1,
	 encode_message/1
	]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(log(Msg), logger:write("codec", Msg)).

parse_lines(Message) ->
    Full_lines = get_full_lines(Message),
    Unfinished_line = get_unfinished_line(Message),
    {Full_lines, Unfinished_line}.

get_full_lines(Message) ->
    Separator = "\r\n",
    Last_separator_start_index = string:rstr(Message, Separator),
    Last_separator_end_index = Last_separator_start_index + 1,
    First_char_index = 1,
    Full_lines = string:substr(Message, First_char_index, Last_separator_end_index),
    re:split(Full_lines, Separator, [{return, list}, trim]).

get_unfinished_line(Message) ->
    get_everything_after_last_separator(Message).

get_everything_after_last_separator(Message) ->
    lists:reverse(geals(lists:reverse(Message))).

geals([]) ->
    [];
geals([$\n, $\r | _]) ->
    [];
geals([C | Rest]) ->
    [C | geals(Rest)].

decode_message(Encoded_message) ->
    case get_command(Encoded_message) of
	"PING :" ->
	    Ping_message = get_parameters(Encoded_message),
	    #message{type=ping, text=Ping_message};
	"NOTICE AUTH :" ->
	    NA_message = remove_leading_colon(get_parameters(Encoded_message)),
	    #message{type=notice_auth, text=NA_message};
	":" ->
	    decode_server_message(get_parameters(Encoded_message));
	_ ->
	    #message{type=unsupported, text=Encoded_message}
    end.

get_command(Text) ->
    get_text_before($:, Text) ++ [$:].

get_text_before(_, []) ->
    [];
get_text_before(Char, [Char|_]) ->
    [];
get_text_before(Char, [L|R]) ->
    [L] ++ get_text_before(Char, R).

get_text_after(Char, [Char|Text]) ->
    Text;
get_text_after(_, []) ->
    [];
get_text_after(Char, [_|Rest]) ->
    get_text_after(Char, Rest).

get_parameters(Message) ->
    get_text_after($:, Message).

decode_server_message(Server_message) ->
    Code = get_code(Server_message),
    Action = get_action_for_message_code(Code),
    Action(Server_message).

get_action_for_message_code(Code) ->
    Actions = #{"JOIN" => fun decode_join_message/1,
		"MODE" => fun decode_mode_message/1,
		"NOTICE" => fun decode_notice_message/1,
		"PRIVMSG" => fun decode_private_message/1,
		"QUIT" => fun decode_quit_message/1,
		"KICK" => fun decode_kick_message/1,
		"PART" => fun decode_part_message/1,
		"NICK" => fun decode_nick_message/1,
		"332" => fun decode_topic_message/1,
		"366" => fun decode_end_of_name_list_message/1,
		"353" => fun decode_name_list_message/1,
		"372" => fun decode_motd/1,
		"404" => fun decode_cannot_send_to_channel/1,
		"433" => fun decode_nick_in_use_message/1},
    Default = fun decode_unsupported_message/1,
    maps:get(Code, Actions, Default).

get_code(Server_message) ->
    [_, Code, _] = get_space_separated_tokens(Server_message, 3),
    Code.

decode_cannot_send_to_channel(Message) ->
    [_, _, _, Channel, CSTCMessage] = get_space_separated_tokens(Message, 5),
    Text = remove_leading_colon(CSTCMessage) ++ " " ++ Channel,
    #message{type = other,
	     text = Text}.

decode_private_message(Message) ->
    [Sender, _, Target, Chat_msg] = get_space_separated_tokens(Message, 4),
    #message{type = privmsg,
	     sender = remove_leading_colon(Sender),
	     receiver = Target,
	     text = remove_leading_colon(Chat_msg)
	    }.

decode_nick_message(Message) ->
    [Sender, _, New_nick] = get_space_separated_tokens(Message, 3),
    #message{type = nick,
	     sender = remove_leading_colon(Sender),
	     text = remove_leading_colon(New_nick)
	    }.

decode_nick_in_use_message(Message) ->
    [Sender, _, Old_nick, Attempted_nick, Error_message] = get_space_separated_tokens(Message, 5),
    ?log("Could not change nickname, " ++ Attempted_nick ++ " already in use."),
    #message{type = nick_in_use,
	     sender = remove_leading_colon(Sender),
	     receiver = Old_nick,
	     text = remove_leading_colon(Error_message)
	    }.

decode_part_message(Message) ->
    [Sender, _, Channel] = get_space_separated_tokens(Message, 3),
    #message{type = part,
	     channel = Channel,
	     sender = Sender
	    }.

decode_kick_message(Message) ->
    [Sender, _Code, Channel, Receiver, Kick_message] = get_space_separated_tokens(Message, 5),
    #message{type = kick,
	     channel = Channel,
	     receiver = Receiver,
	     sender = remove_leading_colon(Sender),
	     text = remove_leading_colon(Kick_message)
	    }.

decode_quit_message(Message) ->	     
    [Sender, _, Quit_msg] = get_space_separated_tokens(Message,3),
    #message{type = quit,
	     sender = remove_leading_colon(Sender),
	     text = remove_leading_colon(Quit_msg)
	    }.

decode_motd(Message) ->
    [Sender, _, Nick, MOTD] = get_space_separated_tokens(Message,4),
    #message{type = motd,
	     sender = remove_leading_colon(Sender),
	     receiver = Nick,
	     text = remove_leading_colon(MOTD)
	    }.

decode_name_list_message(Message) ->
    [Sender, _, Nick, _, Channel, List] = get_space_separated_tokens(Message,6),
    #message{type = namelist,
	     sender = remove_leading_colon(Sender),
	     receiver = Nick,
	     channel = Channel,
	     text = remove_leading_colon(List)
	    }.

decode_end_of_name_list_message(Message) ->
    [Sender, _, Nick, Channel, End_message] = get_space_separated_tokens(Message,5),
    #message{type = end_of_namelist,
	     sender = remove_leading_colon(Sender),
	     receiver = Nick,
	     channel = Channel,
	     text = remove_leading_colon(End_message)
	    }.

decode_topic_message(Message) ->
    [Sender, _, Nick, Channel, Topic] = get_space_separated_tokens(Message, 5),
    #message{type = topic,
	     sender = remove_leading_colon(Sender),
	     receiver = Nick,
	     channel = Channel,
	     text = remove_leading_colon(Topic)
	    }.

decode_join_message(Message) ->
    [Sender, _, Channel] = get_space_separated_tokens(Message, 3),
    #message{type = join,
	     sender = remove_leading_colon(Sender),
	     channel = Channel
	    }.

decode_mode_message(Message) ->
    [Sender, _, Nick, Mode] = get_space_separated_tokens(Message, 4),
    #message{type = mode,
	     sender = remove_leading_colon(Sender),
	     receiver = Nick,
	     text = Mode
	    }.

decode_notice_message(Message) ->
    [Sender, _, Nick, Notice] = get_space_separated_tokens(Message, 4),
    #message{type = notice,
	     sender = remove_leading_colon(Sender),
	     receiver = Nick,
	     text = remove_leading_colon(Notice)
	    }.

decode_unsupported_message(Message) ->
    #message{type = unsupported,
	     text = Message
	    }.

remove_leading_colon([$:|Text]) ->
    Text;
remove_leading_colon(Text) ->
    Text.

text_before_exclamation_mark(Text) ->
    get_text_before($!, Text).

make_printable(Decoded_message) ->
    Type = Decoded_message#message.type,
    Action = get_action_for_making_message_printable(Type),
    Action(Decoded_message).

get_action_for_making_message_printable(Type) ->
    Actions = #{ping => fun make_printable_ping_message/1,
		join => fun make_printable_join_message/1,
		mode => fun make_printable_mode_message/1,
		kick => fun make_printable_kick_message/1,
		privmsg => fun make_printable_privmsg/1,
		quit => fun make_printable_quit_message/1,
		part => fun make_printable_part_message/1,
		nick => fun make_printable_nick_message/1,
		unsupported => fun make_printable_unsupported_message/1},
    Default = fun make_printable_generic_message/1,
    maps:get(Type, Actions, Default).

make_printable_ping_message(_) ->
    "Ping?".

make_printable_join_message(Decoded_message) ->
    Channel = Decoded_message#message.channel,
    Nick = text_before_exclamation_mark(Decoded_message#message.sender),
    Nick ++ " has joined " ++ Channel.

make_printable_mode_message(Decoded_message) ->
    Sender = text_before_exclamation_mark(Decoded_message#message.sender),
    Target = Decoded_message#message.receiver,
    Mode = Decoded_message#message.text,
    Sender ++ " sets mode " ++ Mode ++ " to " ++ Target.

make_printable_kick_message(Decoded_message) ->
    Sender = text_before_exclamation_mark(Decoded_message#message.sender),
    Target = Decoded_message#message.receiver,
    Kick_msg = Decoded_message#message.text,
    Channel = Decoded_message#message.channel,
    Sender ++ " has kicked " ++ Target ++ " from " ++ Channel ++ " (" ++ Kick_msg  ++ ")".

make_printable_privmsg(Decoded_message) ->
    Target = Decoded_message#message.receiver,
    Sender = text_before_exclamation_mark(Decoded_message#message.sender),
    Message = Decoded_message#message.text,
    case get_privmsg_type(Target) of
	channel ->
	    ?TIMESTAMP ++ " " ++ Target ++ " (" ++ Sender ++ ") " ++ Message;
	private ->
	    ?TIMESTAMP ++ " -" ++ Sender ++ "- " ++ Message
    end.

get_privmsg_type([$# | _]) ->
    channel;
get_privmsg_type(_) ->
    private.

make_printable_quit_message(Decoded_message) ->
    Sender = text_before_exclamation_mark(Decoded_message#message.sender),
    Sender ++ " has quit IRC (" ++ Decoded_message#message.text ++ ")".

make_printable_part_message(Decoded_message) ->
    Sender = text_before_exclamation_mark(Decoded_message#message.sender),
    Channel = Decoded_message#message.channel,
    Sender ++ " has left " ++ Channel.

get_space_separated_tokens(String, Number_of_splits) ->
    re:split(String, "\\s", [{return, list}, {parts, Number_of_splits}]).

make_printable_nick_message(Decoded_message) ->
    Old_nick = text_before_exclamation_mark(Decoded_message#message.sender),
    New_nick = Decoded_message#message.text,
    Old_nick ++ " is now called " ++ New_nick ++ ".".

make_printable_unsupported_message(Decoded_message) ->
    "? " ++ Decoded_message#message.text.

make_printable_generic_message(Decoded_message) ->
    Decoded_message#message.text.

encode_message(Message) ->
    Type = Message#message.type,
    case Type of
	privmsg ->
	    encode_standard_message(Message#message.receiver, Message#message.text);
	join ->
	    encode_join_message(Message#message.channel);
	pong ->
	    encode_pong_message(Message#message.text);
	ident ->
	    Nick = Message#message.sender,
	    Realname = Message#message.text,
	    encode_ident_message(Nick, Realname);
	nick ->
	    encode_nick_message(Message#message.text);
	part ->
	    Channel = Message#message.channel,
	    Part_msg = Message#message.text,
	    encode_part_message(Channel, Part_msg);
	quit ->
	    encode_quit_message(Message#message.text)
    end.

encode_nick_message(Nickname) ->
    "NICK :"++Nickname.

encode_part_message(Channel, Message) ->
    "PART " ++ ensure_leading_pound(Channel) ++ " :" ++ Message.

encode_quit_message(Message) ->
    "QUIT :" ++ Message.

encode_standard_message(Receiver, Message) ->
    "PRIVMSG " ++ Receiver ++ " :" ++ Message.

encode_ident_message(Nickname, Realname) ->
    "USER " ++ Nickname ++ " " ++ Nickname ++ " " ++ Nickname ++ " :" ++ Realname.

encode_pong_message(Number) ->
    "PONG :" ++ Number.

encode_join_message(Channel) ->
    "JOIN :" ++ ensure_leading_pound(Channel).

ensure_leading_pound([$#|_] = Text) ->
    Text;
ensure_leading_pound(Text) ->
    [$#|Text].
