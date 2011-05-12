-module(tt).

-export([start/2,pad_input/2,remove_padding/1,strengthen_key/1]).

-import(crypto,[md5/1,aes_cbc_128_encrypt/3,aes_cbc_128_decrypt/3]).


strengthen_key(Key) ->
	strengthen_key(Key,1,65535).

strengthen_key(Key,Count,Count) -> Key;
strengthen_key(Key,Count,Max) -> strengthen_key(md5(Key),Count+1,Max).

pad_input(Text,BitSize) ->
	Size = bit_size(Text),
	PadSize = case case Size < BitSize of
			  true -> BitSize - Size;
			  false -> BitSize rem Size
		  	end of
			0 -> BitSize;
			X -> X
		  end,
	<<Textnum:Size>> =  Text,
	<<Textnum:Size,PadSize:PadSize>>.

remove_padding(Text) ->
	BitSize_except_last = bit_size(Text) - 8,
	<<_:BitSize_except_last,LastByte:8>> = Text,
	ActTextSize = bit_size(Text) - LastByte,
	<<ActTextNum:ActTextSize,_:LastByte>> = Text,
	<<ActTextNum:ActTextSize>>.

start(Text,Password) ->
	
	{A1,A2,A3} = erlang:now(),
	random:seed(A1,A2,A3),
	Ivec = md5(term_to_binary(random:uniform(16#FFFF))),
	
	ActKey = strengthen_key(Password),

	Out = aes_cbc_128_encrypt(ActKey,Ivec,pad_input(list_to_binary(Text),128)),

	io:format("Cipher: ~p,{Key,IVec}:{~p,~p}~n",[Out,ActKey,Ivec]),

	DecOut = aes_cbc_128_decrypt(ActKey,Ivec,Out),

	io:format("Decoded output: ~p~n",[remove_padding(DecOut)]).
