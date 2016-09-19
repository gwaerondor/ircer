-module(codec_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("irc.hrl").

parse_lines_test() ->
    Incoming_data = "First\r\nSecond\r\nThird\r\nHalf",
    Expected = {["First", "Second", "Third"], "Half"},
    Result = codec:parse_lines(Incoming_data),
    ?assertEqual(Expected, Result).

parse_ping_message_test() ->
    Incoming_data = "PING :se.erlnet.com",
    Result = codec:decode_message(Incoming_data),
    Expected = #message{type = ping,
			text = "se.erlnet.com"
		       },
    ?assertEqual(Expected, Result).    

get_space_separated_tokens_test() ->
    Msg = ":- ... it's good to b0rk ...",
    Incoming_data = "port80b.se.quakenet.org 372 Gwaeron2 " ++ Msg,
    Result = codec:get_space_separated_tokens(Incoming_data, 3),
    Expected = ["port80b.se.quakenet.org", "372", "Gwaeron2", Msg],
    ?assertEqual(Expected, Result).

decode_regular_message_test() ->
    Incoming_data = ":Alice!~alice@1.1.1.1 PRIVMSG #erlang :Marco",
    Result = codec:decode_message(Incoming_data),
    Expected = #message{type = privmsg,
			sender = "Alice!~alice@1.1.1.1",
			receiver = "#erlang",
			text = "Marco"
		       },
    ?assertEqual(Expected, Result).

decode_quit_message_test() ->
    Incoming_data = ":Gwaerondor!~Robin@192.176.1.92 QUIT :Signed off",
    Result = codec:decode_message(Incoming_data),
    Expected = #message{type = quit,
			sender = "Gwaerondor!~Robin@192.176.1.92",
			text = "Signed off"
		       },
    ?assertEqual(Expected, Result).

decode_motd_test() ->
    Incoming_data = ":port80b.se.quakenet.org 372 Gwaeron :- ... The use of this server & network is a privilege, not a right.",
    Result = codec:decode_message(Incoming_data),
    Expected = #message{type = motd,
			sender = "port80b.se.quakenet.org",
			receiver = "Gwaeron",
			text = "- ... The use of this server & network is a privilege, not a right."
		       },
    ?assertEqual(Expected, Result).

decode_nick_in_use_message_test() ->
    Incoming_data = ":portlane.se.quakenet.org 433 Aliceold Alicenew :Nickname is already in use.",
    Result = codec:decode_message(Incoming_data),
    Expected = #message{type = nick_in_use,
			sender = "portlane.se.quakenet.org",
			receiver = "Aliceold",
			text = "Nickname is already in use."
		       },
    ?assertEqual(Expected, Result).
			

decode_cannot_send_to_channel_test() ->
    Incoming_data = ":port80b.se.quakenet.org 404 Gwaeron #erlang :Cannot send to channel",
    Result = codec:decode_message(Incoming_data),
    Expected = #message{type = other,
		        text = "Cannot send to channel #erlang"
		       },
    ?assertEqual(Expected, Result).

decode_notice_auth_command_test() ->
    Incoming_data = "NOTICE AUTH :*** Couldn't look up your hostname",
    Result = codec:decode_message(Incoming_data),
    Expected = #message{type = notice_auth,
			text = "*** Couldn't look up your hostname"
		       },
    ?assertEqual(Expected, Result).

decode_join_message_test() ->
    Incoming_data = ":gwaeron!~Robin@192.176.1.92 JOIN #gwaeron",
    Expected = #message{type = join,
			sender = "gwaeron!~Robin@192.176.1.92",
			channel = "#gwaeron"
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

decode_mode_message_test() ->
    Incoming_data = ":gwaeron!~Robin@192.176.1.92 MODE gwaeron +i",
    Expected = #message{type = mode,
			sender = "gwaeron!~Robin@192.176.1.92",
			receiver = "gwaeron",
			text = "+i"
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

decode_part_message_test() ->
    Incoming_data = ":Alice!~alice@192.1.1.1 PART #erlang",
    Expected = #message{type = part,
			sender = "Alice!~alice@192.1.1.1",
			channel = "#erlang"
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).    

decode_notice_message_test() ->
    Incoming_data=":port80b.se.quakenet.org NOTICE gwaeron :on 1 ca 1(4) ft 20(20)",
    Expected = #message{type=notice, sender="port80b.se.quakenet.org",
			receiver="gwaeron", text="on 1 ca 1(4) ft 20(20)"},
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

decode_name_list_test() ->
    Incoming_data=":port80b.se.quakenet.org 353 gwaeron @ #n-next :gwaeron @meroigo Akisto vrklgn Neophos Sephy Smaul +zaken +hanoi @Rockabilly_Joe @Q rotor LJUNGBY kenzoku Eleton",
    Expected = #message{type = namelist,
			sender = "port80b.se.quakenet.org",
		        receiver = "gwaeron", channel="#n-next",
			text = "gwaeron @meroigo Akisto vrklgn Neophos Sephy Smaul +zaken +hanoi @Rockabilly_Joe @Q rotor LJUNGBY kenzoku Eleton"
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

decode_end_of_name_list_test() ->
    Incoming_data = ":port80b.se.quakenet.org 366 gwaeron #n-next :End of /NAMES list.",
    Expected = #message{type=end_of_namelist,
			sender="port80b.se.quakenet.org",
		        receiver="gwaeron",
			channel="#n-next",
			text="End of /NAMES list."
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

decode_topic_test() ->
    Incoming_data = ":port80b.se.quakenet.org 332 gwaeron #n-next :The topic! ^_^",
    Expected = #message{type=topic,
			sender="port80b.se.quakenet.org",
			receiver="gwaeron",
			channel="#n-next",
			text="The topic! ^_^"
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

decode_unsupported_command_test() ->
    Incoming_data = "TEHCOMMAND :port80b.se.quakenet.org",
    Expected = #message{type = unsupported,
		        text = Incoming_data
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

decode_unsupported_code_test() ->
    Incoming_data = ":port80b.se.quakenet.org 9001 gwaeron #n-next :Stuff",
    Expected = #message{type = unsupported,
			text = codec:remove_leading_colon(Incoming_data)
		       },
    Result = codec:decode_message(Incoming_data),
    ?assertEqual(Expected, Result).

get_command_for_ping_test() ->
    Incoming_data = "PING :1122334455",
    Result = codec:get_command(Incoming_data),
    Expected = "PING :",
    ?assertEqual(Expected, Result).

get_params_for_ping_test() ->
    Incoming_data = "PING :irc.ericsson.com",
    Result = codec:get_parameters(Incoming_data),
    Expected = "irc.ericsson.com",
    ?assertEqual(Expected, Result).

encode_ident_message_test() ->
    Ident = #message{type = ident,
		     sender = "Gwaeron",
		     text = "Robin Larsson"
		    },
    Result = codec:encode_message(Ident),
    Expected = "USER Gwaeron Gwaeron Gwaeron :Robin Larsson",
    ?assertEqual(Expected, Result).

encode_nick_message_test() ->
    Nick = #message{type = nick,
		    text = "Gwaeron"
		   },
    Result = codec:encode_message(Nick),
    Expected = "NICK :Gwaeron",
    ?assertEqual(Expected, Result).

encode_quit_message_test() ->
    Message = #message{type = quit,
		       text = "I was using IRCer v. 874442773.01"
		      },
    Result = codec:encode_message(Message),
    Expected = "QUIT :I was using IRCer v. 874442773.01",
    ?assertEqual(Expected, Result).

encode_part_message_test() ->
    Message = #message{type = part,
		       channel = "#erlang",
		       text = "Leaving!"},
    Result = codec:encode_message(Message),
    Expected = "PART #erlang :Leaving!",
    ?assertEqual(Expected, Result).

use_ping_to_make_pong_test() ->
    Ping_message = "PING :1213141516",
    Ping = codec:decode_message(Ping_message),
    Outgoing = #message{type = pong,
			text = Ping#message.text
		       },
    Pong = codec:encode_message(Outgoing),
    ?assertEqual("PONG :1213141516", Pong).

% All the make_printable tests need to be rewritten to
% work with #message records instead of tuples.

make_printable_quit_message_test() ->
    Message = #message{type = quit,
		       sender = "Gwaeron!~1.1.1.1",
		       text = "Logging off."
		      },
    Result = codec:make_printable(Message),
    Expected = "Gwaeron has quit IRC (Logging off.)",
    ?assertEqual(Expected, Result).

make_printable_cannot_send_to_channel_test() ->
    Incoming_data = ":port80b.se.quakenet.org 404 Gwaeron #erlang :Cannot send to channel",
    Decoded_message = codec:decode_message(Incoming_data),
    Result = codec:make_printable(Decoded_message),
    Expected = "Cannot send to channel #erlang",
    ?assertEqual(Expected, Result).

make_printable_channel_test() ->
    Message = #message{type = privmsg,
		       sender = "gwaerondor!~gwaeron@123.123.123.123",
		       receiver = "#erlang",
		       text = "HELLO THIS IS DOG"
		      },
    Now = ?TIMESTAMP,
    Result = codec:make_printable(Message),
    Expected = Now ++ " #erlang (gwaerondor) HELLO THIS IS DOG",
    ?assertEqual(Expected, Result).

make_printable_regular_message_test() ->
    Incoming_data = ":Alice!~alice@1.1.1.1 PRIVMSG #erlang :POLO",
    Decoded_data = codec:decode_message(Incoming_data),
    Result = codec:make_printable(Decoded_data),
    Expected = ?TIMESTAMP ++ " #erlang (Alice) POLO",
    ?assertEqual(Expected, Result).

make_printable_private_message_test() ->
    Message = #message{type = privmsg,
		       sender = "gwaerondor!~gwaeron@1.1.1.1",
		       receiver = "gwaeron",
		       text = "Otototo."
		      },
    Now = ?TIMESTAMP,
    Result = codec:make_printable(Message),
    Expected = Now ++ " -gwaerondor- Otototo.",
    ?assertEqual(Expected, Result).

make_printable_generic_message_test() ->
    Message = #message{type = motd,
		       sender = "erlnet.se",
		       receiver = "Alice",
		       text = "- This is MOTD."
		      },
    Printable_message = codec:make_printable(Message),
    Expected = "- This is MOTD.",
    ?assertEqual(Expected, Printable_message).

make_printable_ping_message_test() ->
    Message = #message{type = ping,
		       text = "6655443322"
		      },
    Result = codec:make_printable(Message),
    Expected = "Ping?",
    ?assertEqual(Expected, Result).

make_printable_join_message_test() ->
    Message = #message{type = join,
		       sender = "gwaeron",
		       channel = "#theChannel"
		      },
    Result = codec:make_printable(Message),
    Expected = "gwaeron has joined #theChannel",
    ?assertEqual(Expected, Result).

make_printable_part_message_test() ->
    Incoming_data = ":Bob!~bob@192.2.2.2 PART #erlang",
    Decoded_data = codec:decode_message(Incoming_data),
    Result = codec:make_printable(Decoded_data),
    Expected = "Bob has left #erlang",
    ?assertEqual(Expected, Result).

make_printable_mode_message_test() ->
    Message = #message{type = mode,
		       sender = "Alice!alice@2.2.2.2",
		       receiver = "Bob",
		       text = "+b"
		      },
    Result = codec:make_printable(Message),
    Expected = "Alice sets mode +b to Bob",
    ?assertEqual(Expected, Result).

make_printable_kick_message_test() ->
    Incoming_data = ":Alice!~alice@2.2.2.2 KICK #erlang Bob :KICK'D",
    Data = codec:decode_message(Incoming_data),
    Result = codec:make_printable(Data),
    Expected = "Alice has kicked Bob from #erlang (KICK'D)",
    ?assertEqual(Expected, Result).

make_printable_unsupported_message_test() ->
    Text = ":port80b.se.quakenet.org 315 gwaeron #n-next :End of /WHO list.",
    Message = #message{type = unsupported,
		       text = Text
		      },
    Result = codec:make_printable(Message),
    Expected = "? " ++ Text,
    ?assertEqual(Expected, Result).  

make_printable_nickname_change_message_test() ->
    Incoming_data = ":ALICENATOR!~alice@1.1.1.1 NICK :Alicetastic",
    Decoded_data = codec:decode_message(Incoming_data),
    Result = codec:make_printable(Decoded_data),
    Expected = "ALICENATOR is now called Alicetastic.",
    ?assertEqual(Expected, Result).

text_before_bang_test() ->
    Text = "This! Is! Sparta!",
    Result = codec:text_before_bang(Text),
    Expected = "This",
    ?assertEqual(Expected, Result).

text_before_bang_without_bang_test() ->
    Text = "Who would like some bangers in the mouth?",
    Result = codec:text_before_bang(Text),
    ?assertEqual(Text, Result).

remove_leading_colon_test() ->				      
    ?assertEqual("Hello", codec:remove_leading_colon(":Hello")),
    ?assertEqual("Good bye", codec:remove_leading_colon("Good bye")),
    ?assertEqual(":See you!", codec:remove_leading_colon("::See you!")).

with_leading_pound_test() ->
    ?assertEqual("#moongoose", codec:with_leading_pound("moongoose")),
    ?assertEqual("#moongoose", codec:with_leading_pound("#moongoose")).

encode_join_with_pound_test() ->
    Outgoing = #message{type = join,
			channel = "#n-next"
		       },
    Result = codec:encode_message(Outgoing),
    Expected = "JOIN :#n-next",
    ?assertEqual(Expected, Result).

encode_join_without_pound_test() ->
    Outgoing = #message{type = join,
			channel = "n-next"
		       },
    Result = codec:encode_message(Outgoing),
    Expected = "JOIN :#n-next",
    ?assertEqual(Expected, Result).

encode_standard_message_test() ->
    Outgoing_message = #message{type = privmsg,
				text = "Hello, this is dog",
				receiver="#erlang"
			       },
    Result = codec:encode_message(Outgoing_message),
    Expected = "PRIVMSG #erlang :Hello, this is dog",
    ?assertEqual(Expected, Result).

encode_pong_message_test() ->
    Outgoing_message = #message{type = pong,
				text = "1234567890"
			       },
    Result = codec:encode_message(Outgoing_message),
    Expected = "PONG :1234567890",
    ?assertEqual(Expected, Result).

-endif.
