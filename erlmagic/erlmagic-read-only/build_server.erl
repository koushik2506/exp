-module(build_server).
-export([make_all/5]).

get_type("void") ->
    "";
get_type(T) ->
    hd(lists:reverse(T)).

param_base_type(T) ->
    Rval = case get_type(T) of
	"Blob" ->
	    "??Blob";
	"ChannelType" ->
	    "ChannelType";
	"Color" ->
	    "Color";
	"Geometry" ->
	    "Geometry";
	"Image" ->
	    "Image";
	"ImageType" ->
	    "ImageType";
	"MagickEvaluateOperator" ->
	    "??MagickEvaluateOperator";
	"PaintMethod" ->
	    "PaintMethod";
	"Quantum" ->
	    "??Quantum";
	"StorageType" ->
	    "??StorageType";
	"bool" ->
	    "ERL_INT_VALUE";
	"char" ->
	    "ERL_INT_VALUE";
	"double" ->
	    "ERL_FLOAT_VALUE";
	"int" ->
	    "ERL_INT_VALUE";
	"std::string" ->
	    "erl_iolist_to_string";
	"std::string&" ->
	    "erl_iolist_to_string";
	"void" ->
	    "void";
	[] ->
	    "void";
	"std::list<Magick::Drawable>" ->
	    "??std::list<Magick::Drawable>";
	"Drawable" ->
	    "??Drawable";
	"GravityType" ->
	    "GravityType";
	"CompositeOperator" ->
	    "CompositeOperator";
	"DrawableAffine" ->
	    "??DrawableAffine";
	"NoiseType" ->
	    "NoiseType";
	"unsigned" ->
	    "??unsigned"
    end,
    case string:substr(Rval, 1, 2) of
	"??" ->
	    %io:format("Rval = ~s~n", [Rval]),
	    throw({"Bad param", Rval});
	_ ->
	    Rval
    end.
	 
param_name(H) ->
    R = chp2:param_name(H),
    case R of
	nil ->
	    nil;
        [$*|_] ->
	    throw({"Bad param", R});
	_ ->
	    string:strip(string:strip(R, both, $&), both, $_)
    end.

make_param(Out_file, L, N, Param_fun) ->
    make_param(Out_file, L, N, Param_fun, [], []).

make_param(_, [], _, _, Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)};
make_param(Out_file, [H|L], N, Param_fun, Acc1, Acc2) ->
    Pname = param_name(H),
    T = get_type(chp2:param_type(H)),
    case param_base_type(chp2:param_type(H)) of 
	"void" ->
	    Acc21 = Acc2,
	    Acc11 = Acc1,
	    N_increment = 1;
	"Color" ->
	    Red_name = Pname ++ "_red",
	    Green_name = Pname ++ "_green",
	    Blue_name = Pname ++ "_blue",
	    Acc212 = [lists:flatten(Param_fun(Out_file, "double", Red_name, "ERL_FLOAT_VALUE", N))|Acc2],
	    Acc213 = [lists:flatten(Param_fun(Out_file, "double", Green_name, "ERL_FLOAT_VALUE", N+1))|Acc212],
	    Acc21 = [lists:flatten(Param_fun(Out_file, "double", Blue_name, "ERL_FLOAT_VALUE", N+2))|Acc213],
	    N_increment = 3,
	    Acc11 = ["ColorRGB(" ++ Red_name ++ "," ++ Green_name ++ "," ++ Blue_name ++ ")" | Acc1];
	"Geometry" ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, "string", Pname, "erl_iolist_to_string", N))|Acc2],
	    N_increment = 1,
	    Acc11 = ["Geometry(" ++ Pname ++ ")" | Acc1];
	"CompositeOperator" ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, "string", Pname, "erl_iolist_to_string", N))|Acc2],
	    Acc11 = ["get_composite(" ++ Pname ++ ")" | Acc1],
	    N_increment = 1;
	"NoiseType" ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, "string", Pname, "erl_iolist_to_string", N))|Acc2],
	    Acc11 = ["get_noise_type(" ++ Pname ++ ")" | Acc1],
	    N_increment = 1;
	"ChannelType" ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, "string", Pname, "erl_iolist_to_string", N))|Acc2],
	    Acc11 = ["get_channel_type(" ++ Pname ++ ")" | Acc1],
	    N_increment = 1;
	"GravityType" ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, "string", Pname, "erl_iolist_to_string", N))|Acc2],
	    Acc11 = ["get_gravity_type(" ++ Pname ++ ")" | Acc1],
	    N_increment = 1;
	"ImageType" ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, "string", Pname, "erl_iolist_to_string", N))|Acc2],
	    Acc11 = ["get_image_type(" ++ Pname ++ ")" | Acc1],
	    N_increment = 1;
	"PaintMethod" ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, "string", Pname, "erl_iolist_to_string", N))|Acc2],
	    Acc11 = ["get_paint_method(" ++ Pname ++ ")" | Acc1],
	    N_increment = 1;
	Pname_type ->
	    Acc21 = [lists:flatten(Param_fun(Out_file, T, Pname, Pname_type, N))|Acc2],
	    Acc11 = [Pname | Acc1],
	    N_increment = 1
    end,
    make_param(Out_file, L, N + N_increment, Param_fun, Acc11, Acc21).

bad_cmd(Command) ->
    lists:member(Command, ["//", "compose", "read"]).


make_one(Row, Out_file, Body_fun, Param_fun, Bad_param_fun) ->
    %io:format("~p~n", [Row]),
    Command = chp2:name(Row),
    try 
	case bad_cmd(Command) of 
	    false ->
		{Param_names, Param_lines} = make_param(Out_file, chp2:param_vals(Row), 3, Param_fun), 
		Body_fun(Out_file, Command, Param_names, Param_lines);
	    true ->
		Bad_param_fun(Out_file, Command, "not implemented")
	end
    catch
	throw:{"Bad param", Reason} ->
	    Bad_param_fun(Out_file, Command, Reason)
    end.

make_all(F, Out_file, Body_fun, Param_fun, Bad_param_fun) ->
    MakeFunc =  fun(Out, Bf, Pf, Bpf) -> (fun(Row) -> make_one(Row, Out, Bf, Pf, Bpf) end) end,
    Func = MakeFunc(Out_file, Body_fun, Param_fun, Bad_param_fun),
    lists:foreach(Func, chp2:parse_file(F)).

	      
