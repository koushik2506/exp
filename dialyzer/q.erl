-module(q).
-export([init/1]).

-record(config, {hostname, port}).

init([Config_record]) ->
    case inet:gethostbyname(Config_record#config.hostname) of
        {ok, {hostent, _HostName,_Unused,inet,_Ver,[IP]}} ->
            HTTP_IP = IP;
        _ ->
            HTTP_IP = {127,0,0,1}
    end,
   {ok,Listen} = gen_tcp:listen(Config_record#config.port,[list,
                                {ip,HTTP_IP},
                                {packet,http},
                                {active, false},
                                {reuseaddr,true}]),
   do_accept(Listen), %% returns immediately
   {ok,none}.

do_accept(_) -> ok.
