-module(chp2).
-export([parse_file/1, name/1, type/1, param_vals/1, modifier/1, param_name/1, param_type/1, test/0]).

type_name_helper(["void"]) ->
    {nil, "void"};
type_name_helper([Name|Type]) ->
    {string:strip(Name), lists:reverse(Type)}.

type_name(L) -> 
    T = string:tokens(string:strip(L), " "),
    T1 = lists:reverse(T),
    type_name_helper(T1).

param(L) ->
    case string:tokens(string:strip(L), "=") of
	[P, Default] ->
	    {type_name(P), string:strip(Default)};
	[P] ->
	    {type_name(P), nil}
    end.
    

params(L) ->
    Params = string:tokens(L, ","),
    lists:map(fun param/1, Params).
    

parse("//"++_) ->
    {"//", "","",""};
parse(L) ->
    case string:tokens(L, "()") of 
	[Type_Name|[Rest]] ->
	    Modifier = nil;
	[Type_Name, Rest, Mod] ->
	    Modifier = string:strip(Mod)
    end,
    {Name, Type} = type_name(Type_Name),
    Params = params(Rest),
    {Name, Type, Modifier, Params}.

do_file(S, Acc) ->
    case io:get_line(S, '') of
	eof -> 
	    lists:reverse(Acc);
	L ->
	    Acc1 = [parse(string:strip(string:strip(string:strip(L),both,$\n),both,$;))|Acc],
	    do_file(S, Acc1)
    end.

parse_file(F) ->
    {ok, S} = file:open(F, read),
    do_file(S, []).

name(X) ->
    element(1, X).

type(X) ->
    element(2, X).

modifier(X) ->
    element(3, X).

param_vals(X) ->
    element(4, X).

param_name(X) ->
    element(1, element(1, X)).

param_type(X) ->
    element(2, element(1, X)).

print_file(F) ->
    lists:foreach(fun(X) -> io:format("~s ~p ~p ~p~n", [name(X), type(X), modifier(X), param_vals(X)]) end, parse_file(F)).

test() ->
    print_file("image.h").
    %parse(" rtype  name(t1 param1, t2 param2)").
     

