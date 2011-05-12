-module(img).

-export([scale/1]).


-define(SCALE_AMOUNT,"90x90").
-define(IMAGE_EXTENSIONS, ["png","jpg","bmp","gif"]).

scale(FileName) ->
	FileNameTokens = string:tokens(FileName,"."),
	[FileHead|_] = FileNameTokens,
	[Ext|_] = lists:reverse(FileNameTokens),
	case lists:member(string:to_lower(Ext),?IMAGE_EXTENSIONS) of
		true ->
			Cmd = lists:append(["convert " ,FileName, " -scale ",?SCALE_AMOUNT," ",FileHead,"-thumb.",Ext]),
			case os:cmd(Cmd) of
				[] -> ok;
				E -> {error,E}
			end;
		false -> not_image
	end.
	
