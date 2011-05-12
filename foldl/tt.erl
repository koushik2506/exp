-module(tt).

-export([test/0]).

test() ->
	A = [1,2,3,4],
	lists:foldr(fun(X,Acc) ->
			[case X of 
			 1 -> a;
			 2 -> b;
			 3 -> c;
			 4 -> d
			end|Acc]
		    end,[],A).
