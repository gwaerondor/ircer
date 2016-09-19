#Ircer
An Erlang IRC client.

## How to use
Compile with rebar.
Start by loading the ircer_app module and connect to a server.

Human interface functions are in the i module, just to make them quicker to access. This means that OTP's i module will be unavailable. Call the exported function ```i:help/0``` to see the available commands.

## Limitations
There is absolutely no UI implemented, so everything is called with erlang commands.
This means that you need to use, for example, i:msg/2 in order to send a simple message to a channel.

This will probably be improved in the future, by storing some state in loopdata, to keep track of the active channel and such.

There are many different codes a message can have when it arrives at the client from the server. I did this as a fun project where I reverse engineered the protocol, so there's tons and tons of codes that are just kind of ignored. You will receive the message and it will be displayed, but the client won't do anything special with it other than displaying it. Lots of the codes I ignored on purpose, and some I couldn't figure out what they were for. However it doesn't seem to affect functionality much.

This will probably never be a _nice_ IRC client though, but that is not the ambition. :) Use Irssi for that.