-module(qq).

-export([start/0,put/2,get/1,receive_loop/1,cursor_process/2,start_list/1,continue_list/2]).
-record(store,{key,value}).

-include_lib("stdlib/include/qlc.hrl").

start() ->
	mnesia:start(),
	case mnesia:wait_for_tables([store],2000) of
		ok -> ok;
		{timeout,_} ->
			mnesia:stop(),
			mnesia:create_schema([node()]), 
			mnesia:start(),
			mnesia:create_table(store,[{type,set},{attributes,record_info(fields,store)},{disc_only_copies,[node()]}])
	end.

put(Key,Value) ->
	mnesia:transaction(fun() -> mnesia:write(#store{key=Key,value=Value}) end).

get(Key) ->
	case mnesia:transaction(fun() -> mnesia:read({store,Key}) end) of
		{atomic,[#store{value=V}]} ->  V;
		{atomic,[]} -> [];
		E -> E
	end.

receive_loop(Cursor) ->
	receive
		{next_answers,Pid,Num} ->
			Pid ! {cursor_output,qlc:next_answers(Cursor,Num)},
			receive_loop(Cursor);
		stop -> qlc:delete_cursor(Cursor);
		_ -> receive_loop(Cursor)
	end.

cursor_process(ParentPID,Num) ->
	mnesia:transaction(fun() ->
				Cursor = qlc:cursor(qlc:q([X||X<-mnesia:table(store,[{n_objects,10}])])),
				ParentPID ! {cursor_output,qlc:next_answers(Cursor,Num)},
				receive_loop(Cursor)
			   end).
					

start_list(N) ->
	Pid = spawn(qq,cursor_process,[self(),N]),
	receive
		{cursor_output,E} -> {Pid,E}
	end.

continue_list(CPid,N) ->
	CPid!{next_answers,self(),N},
	receive
		{cursor_output,E} -> E
	end.
