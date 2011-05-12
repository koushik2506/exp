-module(func).

-export([start/0]).

start() ->
	F1 = fun() -> io:format("f1~n") end,
	F2 = fun() -> io:format("f2~n") end,
	fa(F1,F2).


	
