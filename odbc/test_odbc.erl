-module(test_odbc).
-export([start/0]).

start() ->
	{ok,Ref} = odbc:connect("DSN=mundusync;UID=root;PWD=picopeta",[]),
	Query = "INSERT INTO test(key_txt,value_txt) VALUES(?,?)",
	Param = [{{sql_varchar,10},["a","b","c"]},{{sql_varchar,10},["1","2","3"]}],
	odbc:param_query(Ref,Query,Param).
