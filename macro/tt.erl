-module(tt).

-export([start/0]).


start() ->
-ifdef(SOMETHING).
	io:format("somthing~n"),
-endif.
	io:format("there~n").
