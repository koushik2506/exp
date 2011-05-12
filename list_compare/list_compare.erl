-module(list_compare).

-export([start/2]).


start(List1,List2) ->
	case catch lists:foreach(fun(X) ->
			case lists:member(X,List2) of
				true -> throw(true);
				false -> false
			end
		      end,List1) of
		true -> true;
		_ -> false
	end.
