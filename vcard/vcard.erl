-module(vcard).

-import(helper,[get_child/3]).
-import(xmerl_xs,[select/2,value_of/1]).
-import(devinf,[get_devinf/3]).

-compile(export_all).

% General 

-export([get_vcard_profile/3,string2vcard/1,vcard2string/2]).

% VCARD

-export([get_photo/1,get_formatted_name/1,get_name/1,get_adr/1,get_tel/1,get_email/1]).

% VCALENDAR

-export([get_type/1,get_summary/1,get_starttime/1,get_endtime/1]).

% VMSG

-export([get_vbodyvalue/1,get_vbodydate/1,get_msgbox/1]).

% VCALLLOG

-export([get_vcaltype/1,get_vcalstarttime/1,get_vcalduration/1,get_vcalcalltype/1,get_vcalsize/1,get_vcaltime/1,get_vcalendtime/1]).

% VNOTE

-export([get_vnotecreated/1,get_vnotemodified/1,get_vnotebody/1]).

-include_lib("vcard.hrl").
-include_lib("xmerl.hrl").
-include_lib("devinf.hrl").
-include_lib("mime.hrl").

concat(ListsofLists) -> lists:append(ListsofLists).

get_vcard_profile(Username,Dbname,Devid) ->
	FullProfile = get_devinf(Username,Dbname,Devid),
	DevinfVersion = get_child(FullProfile,"/DevInf/VerDTD",text),
	case DevinfVersion of
		"1.2" ->
			[CTCap|_] = [X|| X <- select("/DevInf/DataStore/CTCap",FullProfile), 
		      		  element(1,X) =:= xmlElement, 
		      		  X#xmlElement.name =:= 'CTCap',
		      		  ((get_child(X,"/CTCap/CTType",text) =:= ?TYPE_CONTACT) or 
				  	(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_CALENDAR) or
					(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_SMS) or
					(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_FOLDER) or
					(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_FILE) or 
					(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_EMAIL) or
					(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_CALLLOG) or
					(get_child(X,"/CTCap/CTType",text) =:= ?TEXT_PLAIN) or
					(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_NOTE) or
					(get_child(X,"/CTCap/CTType",text) =:= ?TYPE_BOOKMARK))],
			PropertyList = [X || X <- select("/CTCap/Property",CTCap),
					     element(1,X) =:= xmlElement,
					     X#xmlElement.name =:= 'Property'],
			[process_property(X) || X <- PropertyList];
		"1.1" ->
			[CTCap|_] = [X||X <- select("/DevInf/CTCap",FullProfile),
				  element(1,X) =:= xmlElement,
				  X#xmlElement.name =:= 'CTCap'],
			create_profile([X|| X <- CTCap#xmlElement.content,element(1,X) =:= xmlElement]);
		 _ ->
		 	throw({error,internal_server_error})
	  end.

	
create_profile(CTCapList) ->
	create_profile(CTCapList,[],[],[]).

create_profile([],_,_,VCardList) -> VCardList;
create_profile([H|T],[],[],[]) when H#xmlElement.name =:= 'CTType' ->
	create_profile(T,[],[],[]);
create_profile([H|T],[],[],VCardList) when  H#xmlElement.name =:= 'PropName' ->
	create_profile(T,pop_value(H),[],VCardList);
create_profile([H|T],PropName,PropList,VCardList) when ((H#xmlElement.name =:= 'ValEnum') or (H#xmlElement.name =:= 'ParamName')) ->
	create_profile(T,PropName,[pop_value(H)|PropList],VCardList);
create_profile([H|T],PropName,PropList,VCardList) when H#xmlElement.name =:= 'PropName' ->
	create_profile([H|T],[],[],[#vcardproperty{name=PropName,value_type=any,types=PropList}|VCardList]);
create_profile([H|_],_,_,VCardList) when H#xmlElement.name =:= 'CTType' ->
	VCardList;
create_profile([_|T],PropName,PropList,VCardList) ->
	create_profile(T,PropName,PropList,VCardList).

process_property(PropertyTag) ->
	Name = get_child(PropertyTag,"/Property/PropName",text),
	{Value_type,Type} = case get_child(PropertyTag,"/Property/ValEnum",text) of
				"no_such_key" -> 
					Type_enum = [pop_value(X) || X <- select("/Property/PropParam/ValEnum",PropertyTag),
						  		    element(1,X) =:= xmlElement,
						  		    X#xmlElement.name =:= 'ValEnum'],
					{any,Type_enum};
				_ -> 
					Type_enum = [pop_value(X) || X <- select("/Property/ValEnum",PropertyTag),
								    element(1,X) =:= xmlElement,
								    X#xmlElement.name =:= 'ValEnum'],
					{fixed,Type_enum}
			   end,
	#vcardproperty{ name = Name,value_type = Value_type,types = Type}.

pop_value(X) ->
	[Y] = value_of(X),
	Y.

string2vcard(String) ->
	ProcessedString = process_str(String),
	StringKeys = string:tokens(ProcessedString,"\r\n"),
	%StringKeys = string:tokens(defold(String),"\r\n"),
	case lists:member("BEGIN:VMSG",StringKeys) of	
		true -> msg2key(StringKeys);
		false -> lists:reverse(tokenize(ProcessedString,name,[],#vcardkey{},[]))  
			 %ProcessedString
	end.

%%% New tokenize from vcard2 %%%

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
	case regexp:match(lists:flatten(TypeList),"QUOTED-PRINTABLE") of
		{match,_,_} ->
			{Rest,Value} = strip_quoted_value(T),
			tokenize(Rest,name,[Vcardkey#vcardkey{value=Value,types=TypeList}|List],#vcardkey{},[]);
		nomatch -> 
			tokenize(T,value,List,Vcardkey#vcardkey{types=[lists:reverse(Buffer)|Etypes]},[])
	end;

%% value over finish vcard

%% The next 3 clauses handle photo data in vcards, which are put in with no regard for field separators

%% Empty value means value starts with \n
tokenize([$\n|T],value,List,Vcardkey,[]) ->
	tokenize(T,value,List,Vcardkey,[$\n]);

%% Every 65 chars of base64 data is separated by \n followed by 3 spaces
tokenize([$\n,32|T],value,List,Vcardkey,Buffer) ->
	tokenize(T,value,List,Vcardkey,[32,$\n|Buffer]);

%% vcardkey actually finishes
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


%%% End new cut from vcard2

msg2key(StringKeys) ->
	msg2key(StringKeys,false,[],[]).

msg2key([],_,Resp,_) -> lists:reverse(Resp);
msg2key([H = "BEGIN:VBODY"|T],false,Resp,Buffer) ->
	msg2key(T,true,[string2key(H)|Resp],Buffer);
msg2key(["END:VBODY"|T],true,Resp,Buffer) ->
	msg2key(T,false,
		concat([[#vcardkey{name="END",types=[],value="VBODY"},
			#vcardkey{name="VBODYVALUE",types=[],value=Buffer}],Resp]),[]);
msg2key([H|T],true,Resp,Buffer) ->
	msg2key(T,true,Resp,concat([Buffer,"\n",H]));
msg2key([H|T],false,Resp,Buffer) ->
	msg2key(T,false,[string2key(H)|Resp],Buffer).
	

string2key(String) ->
	{Name,Value} = case string:tokens(String,":") of
			[N,V] -> {N,V};
			[N] -> {N,[]};
			[N|T] -> {N,concat([lists:flatten(T)])}
		       end,
	[NameAct|Types] = string:tokens(Name,";"),
	#vcardkey{name=NameAct,value=Value,types=Types}.

process_str(String) ->
	case regexp:match(String,"ENCODING=QUOTED-PRINTABLE;") of
		nomatch -> defold(String);
		{match,N,N2} ->
			defold(dequote(String,N+N2))
	end.

dequote(String,Point) ->
	SubStr = string:substr(String,Point,length(String)),
	case regexp:match(SubStr,":") of
		nomatch -> String;
		{match,S1,E1} ->
			case regexp:match(SubStr,"=0D=0A=\r\n\r\n") of
				nomatch -> String;
				{match,S2,E2} ->
					StartNum = S1+ E1,
					EndNum = (S2 + E2) - (StartNum + 2),
					Rquote = dequote(string:substr(SubStr,StartNum,EndNum)),
					BeforeStr = string:substr(String,1,(Point+S1)-1),
					AfterStr = string:substr(String,(Point+S2+E2-1),length(String)),
					concat([BeforeStr,Rquote,"\r\n",AfterStr])
			end
	end.


dequote(Str) ->
	dequote_r(Str,[]).

dequote_r([],Resp) -> lists:reverse(Resp);
%dequote_r([61,10,10|T],Resp) -> dequote_r(T,Resp);
%dequote_r([61,First,Second|T],Resp) ->
%	Sum = hexdigit_value(First)*16 + hexdigit_value(Second),
%	dequote_r(T,[Sum|Resp]);
dequote_r([10|T],Resp) -> dequote_r(T,Resp);
dequote_r([13|T],Resp) -> dequote_r(T,Resp);
dequote_r([H|T],Resp) -> dequote_r(T,[H|Resp]).


defold(String) ->
	lists:reverse(defold(String,[])).

defold([],Resp) -> Resp;
defold([13,10,32,32,32|T],Resp) -> defold(T,Resp); 
defold([13,10,32,32|T],Resp) -> defold(T,Resp); 
defold([13,10,32|T],Resp) -> defold(T,Resp); 
defold([13,10,9|T],Resp) -> defold(T,Resp); 
defold([10,10,32|T],Resp) -> defold(T,Resp);
defold([13,13,32|T],Resp) -> defold(T,Resp);
defold([H|T],Resp) -> defold(T,[H|Resp]). 

vcardtype(Vcard) ->	
	case [X|| X <- Vcard,X#vcardkey.name =:= "BEGIN"] of
		[{vcardkey,"BEGIN","VCARD",_}] -> contact;
		SomethingElse ->
			case lists:member({vcardkey,"BEGIN","VMSG",[]},SomethingElse) of	
				true -> sms;
				false ->
					case lists:member({vcardkey,"BEGIN","VBKM",[]},SomethingElse) of
						true -> bookmark;
						false -> calendar
					end
			end
	end.
		

vcard2string(Vcard,Vcard_profile) ->
	case vcardtype(Vcard) of
		sms -> vmsg2string(Vcard,[]);
		bookmark -> vbkm2string(Vcard);
		_ -> vcard2string(Vcard,Vcard_profile,[])
	end.

vbkm2string(Vcard) ->
	vbkm2string(Vcard,[],false).

vbkm2string([],Resp,_) -> lists:flatten(Resp);
vbkm2string([#vcardkey{name="BEGIN",value="ENV"}|T],Resp,_) ->
	vbkm2string(T,Resp,true);
vbkm2string([#vcardkey{name="END",value="ENV"}|T],Resp,_) ->
	vbkm2string(T,Resp,false);
vbkm2string([_|T],Resp,true) ->	
	vbkm2string(T,Resp,true);
vbkm2string([H|T],Resp,false) ->	
	vmsg2string(T,concat([Resp,concat([H#vcardkey.name,[concat([";",X])|| X <-H#vcardkey.types],":",H#vcardkey.value,[10]])])).


vmsg2string([],Resp) -> lists:flatten(Resp);
vmsg2string([#vcardkey{name="VBODYVALUE",value=Value}|T],Resp) ->
	vmsg2string(T,concat([Resp,Value,"\n"]));
vmsg2string([#vcardkey{value="VBODY",name="BEGIN"}|T],Resp) ->
	vmsg2string(T,concat([Resp,"BEGIN:VBODY"]));
vmsg2string([#vcardkey{value="VMSG",name="END"}|T],Resp) ->
	vmsg2string(T,concat([Resp,"END:VMSG"]));
vmsg2string([H|T],Resp) ->
	vmsg2string(T,concat([Resp,concat([H#vcardkey.name,[concat([";",X])|| X <-H#vcardkey.types],":",H#vcardkey.value,[13,10]])])).
	

vcard2string([],_,Resp) -> lists:flatten(Resp);
vcard2string([H|T],Vcard_profile,Resp) ->
	case [X ||X <-Vcard_profile,H#vcardkey.name =:= X#vcardproperty.name] of
		[] ->
			vcard2string(T,Vcard_profile,Resp);
		_ ->
			[Types_in_profile] = [X#vcardproperty.types || X <- Vcard_profile,X#vcardproperty.name =:= H#vcardkey.name],	
			Allowed_types = case H#vcardkey.name /= "PHOTO" of
						true ->
							case Types_in_profile of
								[] -> H#vcardkey.types;
								_  -> 
							[Y || Z <- Types_in_profile, Y <- H#vcardkey.types, (Y =:= Z) ]
							end;
						false ->
							H#vcardkey.types
					end,
			Name_and_type = concat([H#vcardkey.name,[concat([";",X])|| X <- Allowed_types]]),
			%Value = case H#vcardkey.name =:= "PHOTO" of
			%	 false -> H#vcardkey.value;
			%	 true -> fold(H#vcardkey.value)
			%	end,
			Value = H#vcardkey.value,
			vcard2string(T,Vcard_profile,concat([Resp,Name_and_type,":",Value,[13,10]]))
	end.

fold(Str) ->
	fold(Str,[10,13],0).

fold([],Resp,_) -> lists:reverse([10,13|Resp]); 
fold([H|T],Resp,0) ->
	fold(T,[H,32,32,32|Resp],1);
fold(L,Resp,65) ->
	fold(L,[10,13|Resp],0);
fold([H|T],Resp,Count) ->
	fold(T,[H|Resp],Count+1).

get_photo([]) -> "no_photo";
get_photo([Key = #vcardkey{name="PHOTO"}|_]) ->
	case Key#vcardkey.value of
		[] -> "no_photo";
		_ -> 
			{base64:decode_to_string(Key#vcardkey.value),?GENERIC_CONTACT_IMAGE}
	end;
get_photo([_|T]) ->
	get_photo(T).

get_formatted_name(Vcard) ->
	case [X#vcardkey.value||X<-Vcard,X#vcardkey.name =:= "FN"] of	
		[] ->
			case [X#vcardkey.value||X<-Vcard,X#vcardkey.name =:= "N"] of
				[] -> [];
				[Name] ->
					string:join(lists:reverse(string:tokens(Name,";"))," ");
				Name ->
					string:join(lists:reverse(string:tokens(lists:nth(1,Name),";"))," ")
			end;
		[Name] -> Name
	end.

get_name([]) -> [];
get_name([Key = #vcardkey{name="N"}|_]) -> splitter(Key#vcardkey.value,59);
get_name([_|T]) -> get_name(T).

get_adr(Vcard) ->
	case [{X#vcardkey.value,X#vcardkey.types}||X <- Vcard,X#vcardkey.name =:= "ADR"] of	
		[] -> [];
		Adr_list -> [{splitter(Value,59),Types}|| {Value,Types} <- Adr_list]
	end.

get_tel(Vcard) ->
	[{X#vcardkey.value,X#vcardkey.types}||X <- Vcard,X#vcardkey.name =:= "TEL"].

get_email(Vcard) ->
	[{X#vcardkey.value,X#vcardkey.types}||X <- Vcard,X#vcardkey.name =:= "EMAIL"].

get_type(Vcal) ->
	element(2,list_to_tuple([X#vcardkey.value||X<- Vcal,X#vcardkey.name=:="BEGIN"])).

get_summary(Vcal) ->
	element(1,list_to_tuple([X#vcardkey.value||X<- Vcal,X#vcardkey.name=:="SUMMARY"])).

get_starttime(Vcal) ->
	element(1,list_to_tuple([X#vcardkey.value||X<- Vcal,X#vcardkey.name=:="DTSTART"])).

get_endtime(Vcal) ->
	element(1,list_to_tuple([X#vcardkey.value||X<- Vcal,X#vcardkey.name=:="DTEND"])).

get_vbodyvalue(VMsg) ->
	FullStr = element(1,list_to_tuple([X#vcardkey.value||X<- VMsg,X#vcardkey.name =:= "VBODYVALUE"])),
	lists:last(string:tokens(FullStr,"\n")).

get_vbodydate(VMsg) ->
	FullStr = element(1,list_to_tuple([X#vcardkey.value||X <- VMsg,X#vcardkey.name =:= "VBODYVALUE"])),
	lists:nth(1,string:tokens(FullStr,"\n")).

get_msgbox(VMsg) ->
	element(1,list_to_tuple([X#vcardkey.value||X<- VMsg,X#vcardkey.name =:= "X-IRMC-BOX"])).

get_vcaltype([_|VCallLog]) ->
	case [X#vcardkey.value||X<-VCallLog,X#vcardkey.name =:= "BEGIN"] of
		["VLOGVOICE"] -> "voice";
		["VLOGDATA"] -> "data";
		["VLOGMSG"] -> "message"
	end.

get_vcalstarttime(VCalllog) ->
	case [X#vcardkey.value||X<- VCalllog,X#vcardkey.name =:= "STARTTIME"] of
		[] -> []; 
		[Time|_] -> Time
	end.

get_vcalendtime(VCalllog) ->
	case [X#vcardkey.value||X<- VCalllog,X#vcardkey.name =:= "ENDTIME"] of
		[] -> [];
		[Time|_] -> Time
	end.

get_vcalsize(VCalllog) ->
	case [X#vcardkey.value || X<- VCalllog,X#vcardkey.name =:= "SIZE"] of
		[] -> "0";
		[Time|_] -> Time
	end.

get_vcaltime(VCalllog) ->
	case [X#vcardkey.value || X<- VCalllog,X#vcardkey.name =:= "TIME" ] of
		[] -> [];
		[Time|_] -> Time
	end.

get_vcalduration(VCalllog) ->
	case [X#vcardkey.value||X <- VCalllog,X#vcardkey.name =:= "DURATION"] of
		[] -> "0"; 
		[Time|_] -> Time
	end.

get_vcalcalltype(VCalllog) -> 
	case [X#vcardkey.value||X <- VCalllog,X#vcardkey.name =:= "TYPE" ] of
		["RECEIVED"] -> "received";
		["DIALED"] -> "dialed";
		["MISSED"] -> "missed";
		["SENT"] -> "sent";
		_ -> ""
	end.

get_vnotebody(VNote) ->
	lists:nth(1,[X#vcardkey.value||X<- VNote,X#vcardkey.name =:= "BODY"]).

get_vnotecreated(VNote) ->
	lists:nth(1,[X#vcardkey.value||X<- VNote,X#vcardkey.name =:= "DCREATED"]).

get_vnotemodified(VNote) ->
	lists:nth(1,[X#vcardkey.value||X<- VNote, X#vcardkey.name =:= "LAST-MODIFIED"]).


			
splitter(String,Char) ->
	splitter(String,Char,[],[]).

splitter([],_,Buffer,List) ->
	lists:reverse([lists:reverse(Buffer)|List]);
splitter([Char|T],Char,Buffer,List) ->
	splitter(T,Char,[],[lists:reverse(Buffer)|List]);
splitter([H|T],Char,Buffer,List) ->
	splitter(T,Char,[H|Buffer],List).	


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
	Sum = 16*hexdigit_value(F) + hexdigit_value(D),
	decode_printable(T,[Sum|L]);
decode_printable([H|T],L) -> decode_printable(T,[H|L]).
