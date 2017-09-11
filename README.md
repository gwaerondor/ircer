# Ircer
An Erlang IRC client.

## How to use
Start an Erlang shell and set the path to wherever you have the binaries, for example ```erl -pa /path/to/ircer/ebin/```.
To get the binaries, build with rebar3, e.g. ```rebar3 compile```.

The best way to do all of this in one go is to start a shell directly with rebar3:

```bash
cd /path/to/ircer/
rebar3 shell
```

Start by loading the ircer_app module and calling the start/4 function. This will connect to a server.

```erlang
l(ircer_app).
ircer_app:start("se.quakenet.org", 6667, "MyNickName" "Real Name").
```

Human interface functions are in the ```i``` module, just to make them quicker to access. This means that OTP's i module will be unavailable. Call the exported function ```i:help/0``` to see the available commands.

## Limitations
There is absolutely no UI implemented, so all functionalities are reached with pure Erlang function calls from the Erlang REPL.
This means that you need to use, for example, i:msg/2 in order to send a simple message to a channel.

This will probably be improved in the future, by storing some state in loopdata, to keep track of the active channel and such.

There are many different codes a message can have when it arrives at the client from the server. I did this as a fun project where I reverse engineered the protocol, so there's tons and tons of codes that are just kind of ignored. You will receive the message and it will be displayed, but the client won't do anything special with it other than displaying it. Lots of the codes I ignored on purpose, and some I couldn't figure out what they were for. However it doesn't seem to affect functionality much.

This will probably never be a _nice_ IRC client, but that is not the ambition. :) Use Irssi for that.
