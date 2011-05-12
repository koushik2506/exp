-module(build_cpp).
-export([test/0, start/0]).

param_func(_Out_file, "Image", Pname, _, N) ->
    io_lib:format("\t\tImage& ~s = get_image(~B, msg);~n",[Pname, N]);
param_func(_Out_file, T, Pname, Pname_type, N) ->
    io_lib:format("\t\t~s ~s = ~s(erl_element(~B, msg));~n", [T, Pname, Pname_type, N]).


body_func(Out_file, Command, Param_names, Param_lines) ->
    Command_name = util:make_command_name(Command, Param_names),
    Parameters = util:join(",", Param_names),
    io:format(Out_file, "\telse if (command == \"" ++ Command_name ++ "\") {~n",[]),
    io:format(Out_file, "\t\tImage& image = get_image(2, msg);~n",[]), 
    lists:foreach(fun(L) -> io:format(Out_file, "~s", [L]) end, Param_lines),
    io:format(Out_file, "\t\timage.~s(~s);~n", [Command, Parameters]),
    io:format(Out_file, "\t\terl_send(fd, pid, ok);~n",[]),
    io:format(Out_file, "\t}~n",[]),
    ok.

bad_param_func(Out_file, Command, Reason) ->
    io:format(Out_file, "\t\t//~s not implemented because of parameter ~s~n", [Command, Reason]).

% test is for interactive use
test() ->
    case file:open("im_commands.h", write) of
	{ok, Out_file} ->
	    build_server:make_all("image.h", Out_file, fun body_func/4, fun param_func/5, fun bad_param_func/3);
	{error, Reason} ->
	    io:format("couldn't open file ~p~n", [Reason])
    end.

% start is for running in a script.
start() ->
    test(),
    init:stop().


