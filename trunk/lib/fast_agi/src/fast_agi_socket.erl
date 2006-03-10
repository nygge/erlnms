-module(fast_agi_socket).

-export([start_link/3]).

-export([init/1]).

-record(c,  {sock,
             port,
             peer_addr,
             peer_port
	     }).

start_link(ListenPid, ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, init, [{ListenPid, ListenSocket, ListenPort}]).

init({Listen_pid, Listen_socket, ListenPort}) ->
    case catch gen_tcp:accept(Listen_socket) of
	{ok, Socket} ->
	    %% Send the cast message to the listener process to 
	    %% create a new acceptor
	    fast_agi_server:create(Listen_pid, self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #c{sock = Socket,
                   port = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port},
	    request(C, []);
	Else ->
	    error_logger:error_report([{application, fast_agi},
				       "Accept failed error",
				       io_lib:format("~p",[Else])]),
	    exit({error, accept_failed})
    end.

request(C, Req) ->
    case gen_tcp:recv(C#c.sock, 0, 30000) of
	{ok,Some} ->
	    case string:str(Some,"\n\n") of
		0 ->
		    request(C,Req++Some);
		_N ->
		    RPars=parse_req(Req++Some),
		    handle_req(C,RPars)
	    end;
	error ->
	    io:format("Got error~n",[])
    end.

parse_req(Req) ->
    [parse_line(L)||L<-string:tokens(Req,"\n")].

parse_line(L) ->
    Par=string:sub_word(L,1,$:),
    Val=string:strip(string:sub_word(L,2,$:),both),
    {list_to_atom(Par),Val}.

handle_req(C,RPars) ->
    io:format("handle_req: ~p~n",[RPars]),
    {Mod,Fun}=get_script(RPars),
    case catch Mod:Fun(RPars,C) of
	Res ->
	    io:format("RESULT ~p~n",[Res]),
	    gen_tcp:close(C#c.sock,read_write),
	    Res
    end.
	
get_script(Req) ->
    Script=get_par(agi_network_script,Req),
    list_to_tuple([list_to_atom(T) || T<-string:tokens(Script,"/")]).

get_par(Par,Req) ->
    case lists:keysearch(Par,1,Req) of
	{value,{Par,Value}} ->
	    Value;
	_Notfound ->
	    []
    end.
