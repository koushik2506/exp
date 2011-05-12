-module(syncml_supervisor).
-behaviour(supervisor).

-export([start_link/0,start_child1/0]).

-export([init/1]).


start_link() ->
	io:format("starting~n"),
	{ok,Pid} = supervisor:start_link(?MODULE,[]),
	%%register(supervisor,Pid),
	Pid.

start_child1() ->
%	ChildSpec = ,
%	supervisor:start_child(supervisor,ChildSpec).
	ok.

init([]) -> 
	{ok,{{one_for_one,1,60},[{child1,{child1,start_link,[]},transient,brutal_kill,worker,[child1]}]}}.
