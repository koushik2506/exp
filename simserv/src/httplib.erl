-module(httplib).
-import(calendar_helper,[time_in_words/1,yesterday/1]).
-export([attach_header/4,concat/1,uri_decode/1,attach_header_with_cookie/5,hexdigit_value/1,escape_quote/1,get_key_value/2,
	 attach_header_chunked/4,add_chunked_data/1]).

concat(L) -> lists:append(L).

attach_header(Content,Type,Content_type,Connection) ->
	{Header,Content} = attach_header_split(Content,Type,Content_type,Connection),
	concat([Header,Content]).

attach_header_split(Content,Type,Content_type,Connection) ->
	case Type of
		400 -> Headstr = "HTTP/1.1 400 Bad Request\r\n";
		_  -> Headstr = "HTTP/1.1 200 \r\n"
	end,

	FrmtStr = concat([Headstr,"Server: SIMSERV \r\nDate: ~s\r\nAccept-Ranges: bytes\r\nCache-Control: private\r\nConnection: ~s\r\nContent-Type: ~s\r\nContent-Length: ~p\r\n\r\n"]),

	Today = erlang:universaltime(),
	{lists:flatten(io_lib:format(FrmtStr,[time_in_words(Today),Connection,Content_type,length(Content)])),Content}.


attach_header_with_cookie(Content,Type,Content_type,Cookie,Connection) ->
	case Type of
		400 -> Headstr = "HTTP/1.1 400 Bad Request\r\n";
		_  -> Headstr = "HTTP/1.1 200 \r\n"
	end,

	case Cookie of
		no_cookie ->
			FrmtStr = concat([Headstr,"Server: SIMSERV \r\nDate: ~s\r\nAccept-Ranges: bytes\r\nCache-Control: private\r\nConnection: ~s\r\nContent-Type: ~s\r\nContent-Length: ~p\r\n\r\n~s"]),
			Today = erlang:universaltime(),
			lists:flatten(io_lib:format(FrmtStr,[time_in_words(Today),Connection,Content_type,length(Content),Content]));
		_ ->	
			FrmtStr = concat([Headstr,"Server: SIMSERV \r\nDate: ~s\r\nAccept-Ranges: bytes\r\nCache-Control: private\r\nConnection: ~s\r\nContent-Type: ~s\r\nSet-Cookie: ~s\r\nContent-Length: ~p\r\n\r\n~s"]),
			Today = erlang:universaltime(),
			lists:flatten(io_lib:format(FrmtStr,[time_in_words(Today),Connection,Content_type,format_cookie(Cookie),length(lists:flatten(Content)),Content]))
	end.

to_hex(Num) ->
	lists:flatten(io_lib:format("~.16B",[Num])).

attach_header_chunked(Content,Type,Content_type,Cookie) ->
	case Type of
		400 -> HeadStr = "HTTP/1.1 400 Bad Request\r\n";
		_ -> HeadStr = "HTTP/1.1 200 \r\n"
	end,

	case Cookie of
		no_cookie ->
			FmtStr = concat([HeadStr,"Server: SIMSERV \r\nDate: ~s\r\nAccept-Ranges: bytes\r\nContent-Type: ~s\r\nTransfer-Encoding: chunked\r\n\r\n~s\r\n~s\r\n\r\n"]),
			Today = erlang:universaltime(),
			lists:flatten(io_lib:format(FmtStr,[time_in_words(Today),Content_type,to_hex(length(lists:flatten(Content))),Content]));
		_ -> 
			FmtStr = concat([HeadStr,"Server: SIMSERV \r\nDate: ~s\r\nAccept-Ranges: bytes\r\nSet-Cookie: ~s\r\nContent-Type: ~s\r\nTransfer-Encoding: chunked\r\n\r\n~s\r\n~s\r\n\r\n"]),
			Today = erlang:universaltime(),
			lists:flatten(io_lib:format(FmtStr,[time_in_words(Today),Cookie,Content_type,to_hex(length(lists:flatten(Content))),Content]))
	end.

add_chunked_data(Content) ->
	lists:flatten(io_lib:format("~s\r\n~s\r\n\r\n",[to_hex(length(Content)),Content])).

format_cookie(Cookie) ->
	concat(["SYNCUISESSION=",Cookie,";path=/"]).

hexdigit_value(Str) ->
	case Str of
	 $A -> 10;
	 $B -> 11;
	 $C -> 12;
	 $D -> 13;
	 $E -> 14;
	 $F -> 15;
	Else -> Else - 48
	end.

uri_decode(Str) ->	
	lists:reverse(uri_decode(Str,"")).

uri_decode([],L) -> L;
uri_decode([H,F,S|T],L) ->
	case H of 
	37 ->
		Sum = hexdigit_value(F)*16 + hexdigit_value(S),
		uri_decode(T,[Sum|L]);
	_ -> uri_decode([F,S|T],[H|L])
	end;
uri_decode([H|T],L) -> uri_decode(T,[H|L]);
uri_decode(H,L) -> uri_decode([],[H|L]).

get_mime(Filename) ->
	case get_extension(Filename) of
	"html" -> "text/html";
	"xml" -> "text/xml";
	"css" -> "text/css";
	"js" -> "text/javascript";
	"png" -> "image/png";
	"jpg" -> "image/jpg";
	"gif" -> "image/gif";
	_   -> "application/x-octet-stream"
	end.

get_extension(Filename) ->
	get_extension_r(string:tokens(Filename,".")).

get_extension_r([H|T]) ->
	case length(T) of
	0 -> H;
	_ -> get_extension_r(T)
	end.

push_str(List,[]) -> List;
push_str(List,[H|T]) ->
	push_str([H|List],T).

escape_quote(Str) ->
	lists:reverse(escape_quote(Str,[])).

escape_quote([],List) -> List;
escape_quote([H|T],List) ->
	case H of
	 39 -> escape_quote(T,push_str(List,[92,39]));
	 34 -> escape_quote(T,push_str(List,[92,34]));
	 _ -> escape_quote(T,[H|List])
	end.

get_key_value([],_) -> no_such_key;
get_key_value([H|T],Key) ->
	try regexp:match(H,"=") of
	{match,_,_} ->
	  [Ke,V] = string:tokens(H,"="),
	  case Ke of
	   Key -> uri_decode(V);
	   _ -> get_key_value(T,Key)
	  end;
	_ -> get_key_value(T,Key)
	catch
		_:_ -> get_key_value(T,Key)
	end.
