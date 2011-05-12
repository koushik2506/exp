-module(cc).

-export([start/0]).


start() ->
	A = 2;
	case A of	
	  1 ->
	  2 -> bing;
	  _ -> bong
	end.
