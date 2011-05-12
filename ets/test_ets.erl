-module(test_ets).

-export([start/0]).

-record(store,{key,value}).

-define(MAX,100000).

start() ->
	ets:new(store,[set,public,named_table]),
	ets:insert(store,#store{key=abc,value=123}),
	start_asking(?MAX).

start_asking(0) -> ok;
start_asking(N) ->
	spawn(fun() ->
		case ets:lookup(store,store) of
			[#store{key=abc,value=V}] ->  io:format("~p got ~p~n",[N,V]);
			E -> io:format("~p  got ~p~n",[N,E])
		end
	      end),
	start_asking(N-1).
	
