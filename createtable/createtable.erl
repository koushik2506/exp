-module(createtable).

-export([start/0]).

-define(MAX,10000).

-record(store,{key,value}).

start() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	error_logger:logfile({open,"music.txt"}),
	start_music(?MAX).

start_music(0) -> ok;
start_music(N) ->
	spawn(fun() ->
		Time = erlang:now(),
		mnesia:create_table(list_to_atom("store" ++ integer_to_list(N)),
					[{attributes,record_info(fields,store)},
					 {record_name,store},
					 {ram_copies,[node()]}
					]),
		error_logger:info_msg("Table ~p time diff:~p~n",
					[N,timer:now_diff(erlang:now(),Time)])
	      end),
	start_music(N-1).

	
