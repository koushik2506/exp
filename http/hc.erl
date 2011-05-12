-module(hc).

-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([start/0,async_worker/1]).

-define(MAXSIZE,4194304).


start() ->
	application:start(inets),
	gen_server:start_link({local,hc},hc,[],[]).


init([]) ->
	{ok,}.


handle_call({http_get,URL,FileName},From,State) ->
	spawn(?MODULE,async_worker,[{http_get,URL,FileName,From}]),
	{noreply,State}.

handle_cast(_,State) ->
	{noreply,State}.



async_worker({http_get,URL,FileName,From},Method) ->
	case http:request(Method,{URL,[]},[],[{sync,false}]) of
		{ok,RequestID} ->
			io:format("got requestid:~p~n",[RequestID]),
			receive 
				{http,{_,{{_,200,_},List,Bin}}} ->
					case Method of
						head ->
							{value,{_,Size}} = lists:keysearch("content-length",1,A),
							case list_to_integer(Size) > ?MAX_SIZE of
								
					io:format("success~n"),
					gen_server:reply(From,List);
					%file:write_file(FileName,Bin),
					%gen_server:reply(From,ok);
				S ->
					io:format("got ~p~n",[S]),
					gen_server:reply(From,notok)
			after
				10000 ->
					gen_server:reply(From,timeout)
			end;
		E ->
			io:format("some problem:~p~n",[E]),
			gen_server:reply(From,error)
	end.

handle_info(infinite,State) ->
	receive 
		message_from_god ->
			ok
	end,
	{noreply,State};

handle_info(Something,State) ->
	io:format("received something:~p~n",[Something]),
	{noreply,State}.

code_change(_Vsn,State,_Data) ->
	{ok,State}.

terminate(_Reason,_State) -> ok.
