-module(token).

-export([token_on_string/2]).

token_on_string(String,Token) ->
	token_on_string(String,Token,[]).

token_on_string([],_,Resp) -> lists:reverse(Resp);
token_on_string(String,Token,Resp) ->
	Point = string:str(String,Token),
	case Point of 
	  0 -> lists:reverse([String|Resp]);
	  _ ->
		Piece = string:substr(String,1,(Point+length(Token))-1),
		case length(String) > length(Piece) of
			true ->
				Rest = string:substr(String,Point+length(Token)),
				token_on_string(Rest,Token,[Piece|Resp]);
			false ->
				token_on_string([],Token,[Piece|Resp])
		end
	end.
