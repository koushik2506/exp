-module(util).
-export([join/2, capitalize/1, make_command_name/2, test_join/0]).

join(_, []) ->
    "";
join(Sep, [F|L]) ->
    lists:foldl(fun(X, Acc) -> Acc ++ Sep ++ X end, F, L).

capitalize([]) ->
    [];
capitalize([F|L]) ->
    if F > 96 ->
	    [F - 32] ++ L;
       true ->
	    [F] ++ L
    end.

make_command_name(Command, Params) ->
    Command ++ "_" ++ integer_to_list(length(Params)).

test_join() ->
    L = ["this", "that", "the", "other"],
    join(",", L).
