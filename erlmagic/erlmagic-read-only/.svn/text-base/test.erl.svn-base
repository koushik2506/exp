-module(test).
-compile(export_all).

init() ->	
    Host = imagelib:start(),
    Image = imagelib:read(Host, "test.jpg"),
    io:format("Image = ~p~n", [Image]),
    imagelib:scale(Host, Image, "20%"),
    imagelib:display(Host, Image),
    {Host, Image}.
    
test1() ->
    {Host, Image} = init(),
    imagelib:edge(Host, Image, 10),
    imagelib:display(Host, Image),
    imagelib:negate(Host, Image, 0),
    imagelib:display(Host, Image),
    Image.


test2() ->
    {Host, Image} = init(),
    imagelib:roll(Host, Image, 100, 100),
    imagelib:display(Host, Image),
    Image.

test3() ->
    {Host, Image} = init(),
    imagelib:reduceNoise(Host, Image),
    imagelib:display(Host, Image),
    Image.

test4() ->
    {Host, Image} = init(),
    Image2 = imagelib:read(Host, "test2.jpg"),
    io:format("Image2 = ~p~n", [Image2]),
    imagelib:scale(Host, Image2, "800x800"),
    imagelib:display(Host, Image2),
    imagelib:composite(Host, Image, Image2, 0, 0, "AddCompositeOp"),
    imagelib:display(Host, Image),
    imagelib:composite(Host, Image, Image2, 0, 0, "MinusCompositeOp"),
    imagelib:display(Host, Image),
    Image.

test_del(0, _) ->
    ok;
test_del(N, Host) ->
    Image2 = imagelib:read(Host, "test2.jpg"),
    io:format("Image2 = ~p~n", [Image2]),
    imagelib:scale(Host, Image2, "800x800"),
    io:format("Image2 after scale = ~p~n", [Image2]),
    imagelib:display(Host, Image2),
    imagelib:delete(Host, Image2),
    test_del(N-1, Host).

test5() ->
    {Host, _} = init(),
    test_del(10, Host).

% test writing an image
test6() ->
    {Host, Image} = init(),
    imagelib:display(Host, Image),
    imagelib:write(Host, Image, "test6.jpg").

% montage test
test7() ->
    {Host, Image} = init(),
    Image2 = imagelib:read(Host, "test2.jpg"),
    imagelib:montageImages(Host, [Image, Image2], [{tile, "2x1"}], "montage.jpg"),
    Montage = imagelib:read(Host, "montage.jpg"),
    imagelib:display(Host, Montage).




test8() ->
    {Host, Image} = init(),
    imagelib:level(Host, Image, 10.0, 250.0, 1.0),
    imagelib:display(Host, Image),
    Image.

one_attrib(Attrib) ->
    string:tokens(Attrib, "=").

test9() ->
    {Host, Image} = init(),
    L = lists:map(fun one_attrib/1, string:tokens(imagelib:attribute(Host, Image, "EXIF:*"), "\n")),
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, L).
		    



    
