-module(echo).

-export([start/0,call_port/1]).


start() ->
	spawn(fun() ->
		register(echoss,self()),
		process_flag(trap_exit,true),
		Port = open_port({spawn,"./echo.sh"},[]),
		loop(Port)
	      end).

call_port(Msg) ->
	echoss ! {call,self(),Msg},
	receive 
		{data,A} ->
			A
	end.

loop(Port) ->
	receive 
		{call,Caller,Msg} ->
			Port ! {self(),{command,Msg}},
			receive
				{Port,A} ->
					Caller ! A
			end,
			loop(Port);
		E ->
			io:format("E is ~p~n",[E]),
			loop(Port)
	end.
