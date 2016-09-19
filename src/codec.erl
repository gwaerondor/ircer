-module(codec).
-include("irc.hrl").
-export([
	 decode_message/1,
	 parse_lines/1,
	 get_parameters/1,
	 get_command/1,
	 get_space_separated_tokens/2,
	 make_printable/1,
	 remove_leading_colon/1,
	 with_leading_pound/1,
	 text_before_bang/1,
	 encode_message/1
	]).
-define(log(Msg), logger:write("codec", Msg)).

parse_lines(Message) ->
    % Since rstr() gives the index to where the
    % separator STARTS, we'll compensate for that
    % by adding 1 (to end at \r) or 2 (to start at \n)
    Separator = "\r\n",
    Last_separator_start_index = string:rstr(Message, Separator),
    Index_of_first_char_after_separator = Last_separator_start_index + 2,
    Last_separator_end_index = Last_separator_start_index + 1,
    Rest = string:substr(Message, Index_of_first_char_after_separator),
    First_char_index = 1,
    Full_lines = string:substr(Message,First_char_index, Last_separator_end_index),
    {string:tokens(Full_lines, "\r\n"), Rest}.

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

get_command(Message) ->
    Index_of_separator = string:chr(Message, $:),
    string:substr(Message, 1, Index_of_separator).

get_parameters(Message) ->
    Index_of_separator = string:chr(Message, $:) + 1,
    string:substr(Message, Index_of_separator).

decode_server_message(Server_message) ->

    [_Sender, Code, _Remainder] = get_space_separated_tokens(Server_message, 2),
    case Code of
	"JOIN" ->
	    decode_join_message(Server_message);
	"MODE" ->
	    decode_mode_message(Server_message);
	"NOTICE" ->
	    decode_notice_message(Server_message);
	"PRIVMSG" ->
	    decode_private_message(Server_message);
	"QUIT" ->
	    decode_quit_message(Server_message);
	"KICK" ->
	    decode_kick_message(Server_message);
	"PART" ->
	    decode_part_message(Server_message);
	"NICK" ->
	    decode_nick_message(Server_message);
	"332" ->
	    decode_topic_message(Server_message);
	"366" ->
	    decode_end_of_name_list_message(Server_message);
	"353" ->
	    decode_name_list_message(Server_message);
	"372" ->
	    decode_motd(Server_message);
	"404" ->
	    decode_cannot_send_to_channel(Server_message);
	"433" ->
	    decode_nick_in_use_message(Server_message);
	_ ->
	    #message{type=unsupported, text=Server_message}
    end.

decode_cannot_send_to_channel(Message) ->
    [_, _, _, Channel, CSTCMessage] = get_space_separated_tokens(Message, 4),
    Text = remove_leading_colon(CSTCMessage) ++ " " ++ Channel,
    #message{type = other,
	     text = Text}.

decode_private_message(Message) ->
    [Sender, _, Target, Chat_msg] = get_space_separated_tokens(Message, 3),
    #message{type = privmsg,
	     sender = remove_leading_colon(Sender),
	     receiver = Target,
	     text = remove_leading_colon(Chat_msg)
	    }.

decode_nick_message(Message) ->
    [Sender, _, New_nick] = get_space_separated_tokens(Message, 2),
    #message{type = nick,
	     sender = remove_leading_colon(Sender),
	     text = remove_leading_colon(New_nick)
	    }.

decode_nick_in_use_message(Message) ->
    [Sender, _, Old_nick, Attempted_nick, Error_message] = get_space_separated_tokens(Message, 4),
    ?log("Could not change nickname, " ++ Attempted_nick ++ " already in use."),
    #message{type = nick_in_use,
	     sender = remove_leading_colon(Sender),
	     receiver = Old_nick,
	     text = remove_leading_colon(Error_message)
	    }.

decode_part_message(Message) ->
    [Sender, _, Channel] = get_space_separated_tokens(Message, 2),
    #message{type = part,
	     channel = Channel,
	     sender = Sender
	    }.

decode_kick_message(Message) ->
    [Sender, _Code, Channel, Receiver, Kick_message] = get_space_separated_tokens(Message, 4),
    #message{type = kick,
	     channel = Channel,
	     receiver = Receiver,
	     sender = remove_leading_colon(Sender),
	     text = remove_leading_colon(Kick_message)
	    }.

decode_quit_message(Message) ->	     
    [Sender, _, Quit_msg] = get_space_separated_tokens(Message,2),
    #message{type = quit,
	     sender = remove_leading_colon(Sender),
	     text = remove_leading_colon(Quit_msg)
	    }.

decode_motd(Message) ->
    [Sender, _, Nick, MOTD] = get_space_separated_tokens(Message,3),
    #message{type=motd,
	     sender=remove_leading_colon(Sender),
	     receiver=Nick,
	     text=remove_leading_colon(MOTD)
	    }.

decode_name_list_message(Message) ->
    [Sender, _, Nick, _, Channel, List] = get_space_separated_tokens(Message,5),
    #message{type=namelist,
	     sender=remove_leading_colon(Sender),
	     receiver=Nick,
	     channel=Channel,
	     text=remove_leading_colon(List)
	    }.

decode_end_of_name_list_message(Message) ->
    [Sender, _, Nick, Channel, End_message] = get_space_separated_tokens(Message,4),
    #message{type=end_of_namelist,
	     sender=remove_leading_colon(Sender),
	     receiver=Nick,
	     channel=Channel,
	     text=remove_leading_colon(End_message)
	    }.

decode_topic_message(Message) ->
    [Sender, _, Nick, Channel, Topic] = get_space_separated_tokens(Message, 4),
    #message{type=topic,
	     sender=remove_leading_colon(Sender),
	     receiver=Nick,
	     channel=Channel,
	     text=remove_leading_colon(Topic)
	    }.

decode_join_message(Message) ->
    [Sender, _, Channel] = get_space_separated_tokens(Message, 2),
    #message{type=join,
	     sender=remove_leading_colon(Sender),
	     channel=Channel
	    }.

decode_mode_message(Message) ->
    [Sender, _, Nick, Mode] = get_space_separated_tokens(Message, 3),
    #message{type=mode,
	     sender=remove_leading_colon(Sender),
	     receiver=Nick,
	     text=Mode
	    }.

decode_notice_message(Message) ->
    [Sender, _, Nick, Notice] = get_space_separated_tokens(Message, 3),
    #message{type=notice,
	     sender=remove_leading_colon(Sender),
	     receiver=Nick,
	     text=remove_leading_colon(Notice)
	    }.

remove_leading_colon(Text) ->
    case string:chr(Text, $:) of
	1 ->
	    string:substr(Text, 2);
	_ ->
	    Text
    end.

text_before_bang(Text) ->
    Index_before_bang = string:chr(Text, $!)-1,
    case (Index_before_bang > 0) of
	true ->
	    string:substr(Text,1,Index_before_bang);
	_ ->
	    Text
    end.

make_printable(Decoded_message) ->
    Type = Decoded_message#message.type,
    case Type of
	ping ->
	    "Ping?";
	join ->
	    Channel = Decoded_message#message.channel,
	    Nick = text_before_bang(Decoded_message#message.sender),
	    Nick ++ " has joined " ++ Channel;
	mode ->
	    Sender = text_before_bang(Decoded_message#message.sender),
	    Target = Decoded_message#message.receiver,
	    Mode = Decoded_message#message.text,
	    Sender ++ " sets mode " ++ Mode ++ " to " ++ Target;
	kick ->
	    Sender = text_before_bang(Decoded_message#message.sender),
	    Target = Decoded_message#message.receiver,
	    Kick_msg = Decoded_message#message.text,
	    Channel = Decoded_message#message.channel,
	    Sender ++ " has kicked " ++ Target ++ " from " ++ Channel ++ " (" ++ Kick_msg  ++ ")";
	privmsg ->
	    Receiver = Decoded_message#message.receiver,
	    Sender = text_before_bang(Decoded_message#message.sender),
	    Message = Decoded_message#message.text,
	    case string:chr(Receiver, $#) of
		1 ->
		    Channel = Decoded_message#message.receiver,
		    ?TIMESTAMP ++ " "++Channel++" ("++Sender++") " ++Message;
		_ ->
		    ?TIMESTAMP ++ " -"++Sender++"- " ++Message
	    end;
	quit ->
	    Sender = text_before_bang(Decoded_message#message.sender),
	    Sender ++ " has quit IRC (" ++ Decoded_message#message.text ++ ")";
	part ->
	    Sender = text_before_bang(Decoded_message#message.sender),
	    Channel = Decoded_message#message.channel,
	    Sender ++ " has left " ++ Channel;
	nick ->
	    Old_nick = text_before_bang(Decoded_message#message.sender),
	    New_nick = Decoded_message#message.text,
	    Old_nick ++ " is now called " ++ New_nick ++ ".";
	unsupported ->
	    "? " ++ Decoded_message#message.text;
	_ ->
	    Decoded_message#message.text
    end.

get_space_separated_tokens(String, Number_of_splits) ->
    get_space_separated_tokens(String, Number_of_splits, []).
get_space_separated_tokens(String, 0, Acc) ->
    Acc ++ [String];
get_space_separated_tokens(String, Number_of_splits, Acc) ->
    Index_of_first_space = string:chr(String, $\s),
    Index_of_last_char_before_first_space = Index_of_first_space-1,
    Index_of_first_char_after_first_space = Index_of_first_space+1,
    Index_of_first_letter = 1,
    Token = string:substr(String,
			  Index_of_first_letter,
			  Index_of_last_char_before_first_space
			 ),
    Remainder = string:substr(String, Index_of_first_char_after_first_space),
    get_space_separated_tokens(Remainder, Number_of_splits - 1, Acc ++ [Token]).

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
    "PART " ++ with_leading_pound(Channel) ++ " :" ++ Message.

encode_quit_message(Message) ->
    "QUIT :" ++ Message.

encode_standard_message(Receiver, Message) ->
    "PRIVMSG " ++ Receiver ++ " :" ++ Message.

encode_ident_message(Nickname, Realname) ->
    "USER " ++ Nickname ++ " " ++ Nickname ++ " " ++ Nickname ++ " :" ++ Realname.

encode_pong_message(Number) ->
    "PONG :" ++ Number.

encode_join_message(Channel) ->
    "JOIN :" ++ with_leading_pound(Channel).

with_leading_pound(Text) ->
    case string:chr(Text, $#) of
	1 ->
	    Text;
	_ ->
	    "#" ++ Text
    end.
