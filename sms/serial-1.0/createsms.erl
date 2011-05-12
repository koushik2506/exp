-module(createsms).

-export([create/2,create_hexdump/2,create_gsm_header/2,create_wdp_header/2,send_sms/2]).

-define(WSP_HEADER,"5E0601B6").
-define(GSM_HEAD_OPTIONS,"00410B0C91").
-define(WDP_HEADER_FIXED,"0B05040B8423F000035E").
-define(PACKET_SIZE,200).


send_sms(PduList,Device) ->
	Port = serial:start([],Device),
	Port!{connect},
	Port!{send,["atz\r\n"]},
	timer:sleep(500),
	Port!{send,["at+cmgf=0\r\n"]},
	timer:sleep(1000),
	send_sms(PduList,Port).

send_sms([],Port) -> Port!{disconnect};
send_sms([H|T],Port) ->
	L = round(length(H)/2) - 1,
	Port!{send,["at+cmgs="++integer_to_list(L)++"\r\n"]},
	timer:sleep(1000),
	send_sms_r(H,Port),
	send_sms(T,Port).

send_sms_r([],Port) -> Port ! {send,[[26]]},timer:sleep(10000);
send_sms_r([H|T],Port) ->
	Port!{send,[[H]]},
	send_sms_r(T,Port).
	


create(Wbxml,Number) ->
	{NumofPackets,Packets} = create_hexdump(Wbxml,?PACKET_SIZE),
	{_,_,OutList} = lists:foldr(fun(Packet,{Index,Count,Rest}) ->
					WDP_header = create_wdp_header(Count,Index),
					OverallPacket =
					case Index of
						1 ->
				 			WDP_header ++ ?WSP_HEADER ++ Packet;
						_ ->
				 			WDP_header ++ Packet
					end,
					GSM_header = create_gsm_header(Number,round(length(OverallPacket)/2)),
					{Index+1,Count,[GSM_header ++ OverallPacket|Rest]}
		     		     end,{1,NumofPackets,[]},Packets),
	lists:reverse(OutList).

replace_pad_zero([D]) ->
  lists:foldr(fun(Y,Ecc) ->
		case Y of
		  [32] -> "0" ++ Ecc;
		  E -> [E] ++ Ecc
		end
	      end,[],D).


create_hexdump(Bin,PacketSize) ->
	case
	lists:foldl(fun(X,{Count,Current,Full}) ->
			Rd = replace_pad_zero(io_lib:format("~2.16B",[X])),
			case length(Current) =< PacketSize of
				true ->
					{Count,Current ++ Rd,Full};
				false ->
					{Count+1,Rd,[lists:flatten(Current)|Full]}
			end
		    end,{0,[],[]},binary_to_list(Bin)) of
		{Count,[],Act} -> {Count,Act};
		{Count,Rest,Act} -> {Count+1,[lists:flatten(Rest)|Act]}
	end.


create_gsm_header(Number,PacketSize) ->
	{_,ReversNum} = lists:foldr(fun(X,{Curr,Acc}) ->
				  case Curr of
				  	[] -> {X,Acc};
					_ -> {[],[Curr,X]++Acc}
				  end
				end,{[],[]},Number),
	lists:flatten(io_lib:format("~s~s~s~s",[?GSM_HEAD_OPTIONS,ReversNum,"00F5",replace_pad_zero(io_lib:format("~2.16B",[PacketSize]))])).

create_wdp_header(NumOfPackets,PacketNum) ->
	lists:flatten(io_lib:format("~s~s~s",[?WDP_HEADER_FIXED,replace_pad_zero(io_lib:format("~2.16B",[NumOfPackets])),
								replace_pad_zero(io_lib:format("~2.16B",[PacketNum]))])).
