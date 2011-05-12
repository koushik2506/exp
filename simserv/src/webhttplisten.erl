-module(webhttplisten).
-behaviour(gen_server).
-export([start/1,receive_packet/2,stop/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-import(httplib,[attach_header/4,concat/1,separate_payload/1,get_content_length/1,get_mime/1,get_cookie/1,get_key_value/2,
		 get_content_type/1,get_connection_type/1,attach_header_split/4,attach_header_with_cookie/5,
		 add_chunked_data/1,attach_header_chunked/4]).
-import(translate,[convert/4]).

-record(request,{cookie,url}).

-include_lib("json.hrl").

dev() ->
	logger:start_log("."),
	logger:create_channel(webhttpmessages).

start([Hostname,Port]) -> 
	dev(),
	gen_server:start({local,webhttplisten},webhttplisten,[Hostname,Port],[]).

stop() -> gen_server:call(webhttplisten,stop).

init([Hostname,Port]) ->
	case inet:gethostbyname(Hostname) of
	 {ok, {hostent, _HostName, _Unused, inet, _Ver, [IP]}} ->
		            HTTP_IP = IP;
         _ ->
                HTTP_IP = {127,0,0,1}
    	end,
	{ok,Listen} = gen_tcp:listen(Port,[list,
				          {ip,HTTP_IP},			
					  {packet,http},
					  {active, false},
					  {reuseaddr,true}
				      ]),
	do_accept(Listen),
	{ok,none}.

do_accept(Listen) ->
	gen_server:cast(webhttplisten,{accept,{Listen}}).

wrap_process(Socket,Request) ->
	wrap_process(Socket,Request,1).

wrap_process(Socket,Request,Count) ->
	Response = process_get_request(Request,Count),
	case Response of
		nothing -> void;
		_ -> gen_tcp:send(Socket,Response)
	end,
	case Count<10 of
		false -> gen_tcp:close(Socket);
		true -> wrap_process(Socket,Request,Count+1)
	end.


receive_packet(Socket,Request) ->
	case catch gen_tcp:recv(Socket,0) of % case tcp recv
		{ok,{http_request,'GET',{abs_path,L},_Vsn}} ->
			receive_packet(Socket,Request#request{url=L});
		{ok,{http_header,_,'Cookie',_,Value}} -> 
			receive_packet(Socket,Request#request{cookie=Value});
		{ok,{http_header,_,'cookie',_,Value}} ->
			receive_packet(Socket,Request#request{cookie=Value});
		{ok,{http_header,_,_,_,_}} -> 
			receive_packet(Socket,Request);
		{ok,http_eoh} ->
			wrap_process(Socket,Request);
		{ok,_Msg} ->
			% bad request %
			gen_tcp:close(Socket)
	end. % end for tcp recv

process_get_request(#request{url=URL,cookie=Cookie},Count) ->
	Att = string:tokens(URL,"?&"),
	OutputMime = "text/html",
	{Response,RespCookie,Translate} = 
	case get_key_value(Att,"rqty") of
		no_such_key ->
			{"Invalid command",no_cookie,false};
		"do" ->
			io:format("got do~n"),
			case Count>10 of
			 false ->
				Timeout = list_to_integer(get_key_value(Att,"timeout")),
				receive
				after Timeout*1000 ->
					{do_ok,no_cookie,true}
				end;
			 true ->
			 	{[],no_cookie,false}
			end;
		"send" ->
			case get_key_value(Att,"close") of
				"true" -> {nothing,no_cookie,false};
				"false" -> {send_ok,no_cookie,true}
			end;
		 _ -> {nothing,no_cookie,false}
	end,
	TResponse = case Translate of
			true -> convert(Response,?JSON_TYPE,get_key_value(Att,"debug"),get_key_value(Att,"cb"));
			false -> Response
		    end,
	case TResponse of 
		nothing -> nothing;
		_ -> case Count of
			1 -> attach_header_chunked(TResponse,200,OutputMime,RespCookie);
			_ -> add_chunked_data(TResponse)
		     end
	end.

handle_cast({accept,{Listen}},State) ->
	try gen_tcp:accept(Listen) of	
		{ok,Socket} ->
			spawn(webhttplisten,receive_packet,[Socket,#request{}]),
			do_accept(Listen);
		_ -> do_accept(Listen)
	catch
		_:_ -> do_accept(Listen)
	end,
	{noreply,State};

handle_cast(_Msg,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.
handle_call(stop,_From,State) ->
	{stop,normal,stopped,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn,State,_Extra) -> {ok,State}.
