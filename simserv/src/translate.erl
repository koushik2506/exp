-module(translate).
-import(httplib,[concat/1,escape_quote/1,hexdigit_value/1]).
-export([convert/4]).

-include_lib("json.hrl").


convert(Response,Type,Debug,Handler) ->
	TypeResponse = lists:flatten(convert(Response,Type)),
	case Handler of
		no_such_key ->
			TypeResponse;	
		_ ->
			case Debug of
				no_such_key ->
					"<script language=\"javascript\">" ++ 
					lists:flatten(io_lib:format(concat([Handler,"(~s);"]),[TypeResponse])) ++
					"</script>";
				_ ->
					"<script language=\"javascript\">" ++ 
					lists:flatten(
					io_lib:format(concat([Handler,"(~s);",Handler,"_debug('~s');"]),[TypeResponse, 
									escape_quote(lists:flatten(TypeResponse))])) ++
					"</script>"
			end
	end.

convert(do_ok,?JSON_TYPE) ->
	?DO_OK;

convert(send_ok,?JSON_TYPE) ->
	?SEND_OK.

