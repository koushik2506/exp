-module(logger).
-behaviour(gen_server).
-import(httplib,[concat/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_log/1,write_log/2,create_channel/1]).

start_log(Logdir) ->
	gen_server:start_link({local,logger},logger,[Logdir],[]).

init(Logdir) ->
	Tid = ets:new(table,[]),
	{ok,{Logdir,Tid}}.

create_channel(Channel) ->
	gen_server:call(logger,{create_channel,Channel}).

write_log(Channel,Msg) ->
	gen_server:cast(logger,{write,Channel,Msg}).

handle_call({create_channel,Channel},_From,{Logdir,State}) ->
  case ets:lookup(State,Channel) of
   [] ->
	Filename = concat([Logdir,"/",atom_to_list(Channel),"-channel.log"]),
	{ok,FHandle} = file:open(Filename,write),
	ets:insert(State,{Channel,FHandle}),
	{reply,ok,{Logdir,State}};
   _ -> {reply,channel_already_exists,{Logdir,State}}
 end.

handle_cast({write,Channel,Msg},{Logdir,State}) ->
	case ets:lookup(State,Channel) of
	 [{_,Handle}] -> io:format(Handle,"~s~n",[Msg]);
	 _ -> 
	 	Filename = concat([Logdir,"/",atom_to_list(Channel),"-channel.log"]),
	 	{ok,FHandle} = file:open(Filename,write),
	 	ets:insert(State,{Channel,FHandle}),
		io:format(FHandle,"~s~n",[Msg])
	end,
	{noreply,{Logdir,State}}; 

handle_cast(stop,{_,State}) -> 
	ets:close(State),
	{stop,normal,State};

handle_cast(_Msg,State) -> {noreply,State}.
handle_info(_Msg,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn,State,_Extra) -> {ok,State}.
