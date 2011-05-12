-module(spawn).

-export([start/1]).


start(Max) ->
	start(1,Max).

make_request(Number) ->
	Time = erlang:now(),
	http:request("http://stress.geodesic.net:9999/?rqty=login&hlr=__handler&wim_login=user97@hi5.com&wim_pass=pass97&wim_mode=visible&tab=1"),
	Time2 = erlang:now(),
	error_logger:info_msg("request number ~p response time :~p~n",[Number,timer:now_diff(Time2,Time)/1000]).

start(N,N) -> ok;
start(N,Max) ->
	spawn(fun() -> make_request(N) end),
	timer:sleep(125),
	start(N+1,Max).
	
