-module(vcard).

-include_lib("vcard.hrl").

-export([string2vcard/1,decode_printable/1,strip_quoted_value/1]).


string2vcard(Str) ->
	lists:reverse(tokenize(Str,name,[],#vcardkey{},[])).


% finish clauses

tokenize([],_,List,#vcardkey{},[]) -> List;

tokenize([],_,List,CurrentKey,[]) ->
	[CurrentKey|List];

tokenize([],_,List,CurrentKey,Buffer) ->
	[CurrentKey#vcardkey{value=lists:reverse(Buffer)}|List];

tokenize([$\n],_,List,CurrentKey,Buffer) ->
	[CurrentKey#vcardkey{value=lists:reverse(Buffer)}|List];

% expecting name to start

% with type
tokenize([$;|T],name,List,VcardKey,Buffer) ->
	tokenize(T,type,List,VcardKey#vcardkey{name=lists:reverse(Buffer),types=[]},[]);

% without type
tokenize([$:|T],name,List,VcardKey,Buffer) ->
	tokenize(T,value,List,VcardKey#vcardkey{name=lists:reverse(Buffer),types=[]},[]);

% expecting types

tokenize([$;|T],type,List,VcardKey = #vcardkey{types=Etypes},Buffer) ->
	tokenize(T,type,List,VcardKey#vcardkey{types=[lists:reverse(Buffer)|Etypes]},[]);

%% types over
tokenize([$:|T],type,List,Vcardkey = #vcardkey{types=Etypes},Buffer) ->
	TypeList = [lists:reverse(Buffer)|Etypes],
	case lists:member("QUOTED-PRINTABLE",TypeList) of
		true ->
			{Rest,Value} = strip_quoted_value(T),
			tokenize(Rest,name,[Vcardkey#vcardkey{value=Value,types=TypeList}|List],#vcardkey{},[]);
		false -> 
			tokenize(T,value,List,Vcardkey#vcardkey{types=[lists:reverse(Buffer)|Etypes]},[])
	end;

%% value over finish vcard
tokenize([$\n|T],value,List,Vcardkey,Buffer) ->
	tokenize(T,name,[Vcardkey#vcardkey{value=lists:reverse(Buffer)}|List],#vcardkey{},[]);

tokenize([H|T],Expect,List,CurrentKey,Buffer) ->
	tokenize(T,Expect,List,CurrentKey,[H|Buffer]).


strip_quoted_value(T) ->
	strip_quoted_value(T,[],undefined).

strip_quoted_value([$\n|T],List,$=) ->
	strip_quoted_value(T,[$\n|List],$\n);

strip_quoted_value([$\n|T],List,_) ->
	{T,lists:reverse(List)};

strip_quoted_value([H|T],List,_) ->
	strip_quoted_value(T,[H|List],H).


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

decode_printable(Str) ->
	decode_printable(Str,[]).	

decode_printable([],L) -> lists:reverse(L);
decode_printable([$=,$\n|T],L) -> decode_printable(T,L);
decode_printable([$=,F,D|T],L) ->
	Sum = 16*hexdigit_value(F) + D,
	decode_printable(T,[Sum|L]);
decode_printable([H|T],L) -> decode_printable(T,[H|L]).
