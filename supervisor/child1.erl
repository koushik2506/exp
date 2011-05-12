-module(child1).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/0,start_link/0,get_key/0,crash_key/0]).

start() ->
	gen_server:start_link({local,child1},child1,[],[]).

start_link() ->
	%%process_flag(trap_exit,true),
	gen_server:start_link({local,child1},child1,[],[]).

init(_) ->
	io:format("child init~n"),
	{ok,self()}.

get_key() ->
	gen_server:call(child1,{get_key}).

crash_key() ->
	gen_server:call(child1,{crash_key}).

handle_call({get_key},From,State) ->
	self()!{get_key,From},
	{noreply,State}.

handle_info({get_key,From},State) ->
	gen_server:reply(From,State),
	{noreply,State};

handle_info(_Msg,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
handle_cast(_Msg,State) -> {noreply,State}.
code_change(_OldVsn,State,_Extra) -> {ok,State}.
