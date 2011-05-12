-module(gg).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([start/0]).


start() ->
	gen_server:start(?MODULE,[],[]).


init(_) ->
	io:format("GG start~n"),
	{ok,[],1000}.


handle_call(Msg,_From,State) ->
	io:format("GG call: ~p~n",[Msg]),
	{reply,ok,State,1000}.

handle_cast(Msg,State) ->
	io:format("GG cast: ~p~n",[Msg]),
	{noreply,State,1000}.

handle_info(Msg,State) ->
	io:format("GG info: ~p~n",[Msg]),
	{noreply,State,1000}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,_NewVsn,State) -> {ok,State}.
