-module(tt).

-export([start/0]).

start() ->
	String = get_chars([]),
	io:format("String is ~p~n",[String]).

get_chars(List) ->
	[S] = io:get_chars("\b",1),
	case S of
	     10 -> lists:reverse(List);
	     _ -> get_chars([S|List])
	end.
	
