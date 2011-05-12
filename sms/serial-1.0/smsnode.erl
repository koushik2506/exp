-module(smsnode).
-behaviour(gen_server).


% gen_server exports

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


% other

-export([start/0]).

% includes


start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_) ->
	{ok,none}.

handle_cast({send_sms,Wb,Number},State) ->
	createsms:send_sms(createsms:create(list_to_binary(Wb),Number)),
	{noreply,State};

handle_cast(Cast,State) -> 
	{noreply,State}.

handle_call(alive,_From,State) ->
	{reply,yes_alive,State};

handle_call(_Call,_From,State) -> {reply,notdefined,State}.

handle_info(_Msg,State) -> {noreply,State}.

terminate(_Reason,_State) -> ok.
code_change(_OldVsn,StateData,_Extra) -> {ok,StateData}.
