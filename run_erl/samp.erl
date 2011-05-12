-module(samp).

-export([start/0]).


start() ->
	start(100).

start(0) -> ok;
start(N) ->
	io:format("Message~p~n",[N]),
	timer:sleep(1000),
	start(N-1).
