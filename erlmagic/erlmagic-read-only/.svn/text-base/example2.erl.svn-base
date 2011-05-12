-module(example2).
-compile(export_all).

make_thumbnail_name(Name) ->
    T = string:tokens(Name, "."),
    hd(T) ++ "_thn." ++ hd(tl(T)).

do_one_thumbnail(Source_dir, Dest_dir, Host, Name) ->
    Image = imagelib:read(Host, filename:join(Source_dir, Name)),
    imagelib:scale(Host, Image, "10%"),
    imagelib:border(Host, Image, "1x1"),
    imagelib:sharpen(Host, Image, 0.0, 1.0),
    Thumbnail_name = make_thumbnail_name(Name),
    imagelib:write(Host, Image, filename:join(Dest_dir, Thumbnail_name)),
    Thumbnail_name.

do_one_image(Source_dir, Dest_dir, Host, Name) ->
    Image = imagelib:read(Host, filename:join(Source_dir, Name)),
    imagelib:scale(Host, Image, "30%"),
    imagelib:border(Host, Image, "2x2"),
    imagelib:sharpen(Host, Image, 0.0, 1.0),
    imagelib:write(Host, Image, filename:join(Dest_dir, Name)).

do_one(Source_dir, Dest_dir, Host, Name) ->
    Thumbnail_name = do_one_thumbnail(Source_dir, Dest_dir, Host, Name),
    do_one_image(Source_dir, Dest_dir, Host, Name),
    {Thumbnail_name, Name}.
    

do_html_one(Out_file, T) ->
    io:format(Out_file, "<td>~n", []),
    io:format(Out_file, "<table width=150 border=1 cellpadding=1 cellspacing=0 bordercolor=\"#CBCBCD\">~n", []),
    io:format(Out_file, "<tr><td>~n", []),
    io:format(Out_file, "<table cellpadding=\"0\" cellspacing=\"0\" border=\"0\">~n", []),
    io:format(Out_file, "<tr align=middle><td>~n", []),
    io:format(Out_file, "<a href=\"~s\"><img src=\"~s\"~n", [element(2, T), element(1, T)]),
    io:format(Out_file, "hspace=4 vspace=4 border=0></a>~n", []),
    io:format(Out_file, "</td></tr><tr><td><font face=\"Verdana,Sans-serif\" size=\"1\" color=\"#000000\">~n", []),
    io:format(Out_file, "</tr>~n", []),
    io:format(Out_file, "</table>~n", []),
    io:format(Out_file, "</td></tr>~n", []),
    io:format(Out_file, "</table>~n", []),
    io:format(Out_file, "</td>~n", []).
    
    
do_html(_, [], _) ->
    ok;
do_html(Out_file, [T|L], N) when N == 0 ->
    io:format(Out_file, "<tr>~n", []),
    do_html_one(Out_file, T),
    do_html(Out_file, L, N+1);
do_html(Out_file, [T|L], N) when N == 3 ->
    do_html_one(Out_file, T),
    io:format(Out_file, "</tr>~n", []),
    do_html(Out_file, L, 0);
do_html(Out_file, [T|L], N) ->
    do_html_one(Out_file, T),
    do_html(Out_file, L, N+1).

do_html(Out_file, L) ->
    do_html(Out_file, L, 0).
    
write_html(Dest_dir, Names) ->
    case file:open(filename:join(Dest_dir, "photos.html"), write) of
	{ok, Out_file} ->
	    io:format(Out_file, "<html><body><table>~n", []),
	    do_html(Out_file, Names),
	    io:format(Out_file, "</table></body></html>~n", []);
	{error, Reason} ->
	    io:format("couldn't possibly open file ~p~n", [Reason])
    end.

start(Source_dir, Dest_dir) ->
    MakeDoOne = fun(Source, Dest, Host) -> (fun(N) -> do_one(Source, Dest, Host, N) end) end,
    DoOne = MakeDoOne(Source_dir, Dest_dir, imagelib:start()),
    Names = lists:map(DoOne, [filename:basename(X) || X <- filelib:wildcard(Source_dir ++ "/*.jpg")]),
    write_html(Dest_dir, Names).
    
 
test() ->
    Source_dir = ".",
    Dest_dir = ".",
    start(Source_dir, Dest_dir).
			
