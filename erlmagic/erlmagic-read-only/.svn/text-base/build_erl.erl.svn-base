-module(build_erl).
-export([test/0, start/0]).

param_func(_Out_file, _T, _Pname, _Pname_type,_N) ->
    "".

one_param(Param) ->
    Toks = string:tokens(Param, "()"),
    case length(Toks) of
	1 ->
	    util:capitalize(hd(Toks));
	2 ->
	    util:join(",", lists:map(fun util:capitalize/1, string:tokens(hd(tl(Toks)), ",")))
    end.

body_func(Out_file, Command, Param_names, _Param_lines) ->
    %Parameters = util:join(",", Param_names),
    Parameters = case length(Param_names) of
		     0 ->
			 "";
		     _ ->
			 ","++util:join(",",lists:map(fun one_param/1, Param_names))
		 end,
    io:format(Out_file, "~s(Host, Image~s) ->~n", [Command, Parameters]),
    io:format(Out_file, "\trpc(Host, {~s,Image~s}).~n~n",[util:make_command_name(Command, Param_names), Parameters]),
    ok.

bad_param_func(Out_file, Command, Reason) ->
    io:format(Out_file, "%%~s not implemented because of parameter ~s~n", [Command, Reason]).

do_in_file(In_file, Out_file) ->
    case io:get_line(In_file, '') of
	eof ->
	    io:format(Out_file, "~n", []);
	L ->
	    io:format(Out_file, "~s", [L]),
	    do_in_file(In_file, Out_file)
    end.

do_in_file(Out_file) ->
    case file:open("imagelib.in", read) of
	{ok, S} ->
	    do_in_file(S, Out_file);
	{error, Reason} ->
	    io:format("couldn't open input file ~p~n", [Reason])
    end.

% test is for interactive use
test() ->
    case file:open("imagelib.erl", write) of
	{ok, Out_file} ->
	    do_in_file(Out_file),
	    build_server:make_all("image.h", Out_file, fun body_func/4, fun param_func/5, fun bad_param_func/3);
	{error, Reason} ->
	    io:format("couldn't open file ~p~n", [Reason])
    end.

% start is for running in a script.
start() ->
    test(),
    init:stop().


