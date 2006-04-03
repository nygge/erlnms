%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : gen_socket_server.erl
%%% @author Anders Nygren <anders@molloy>
%%% @doc 
%%% @end 
%%% Created :  2 Apr 2006 by Anders Nygren <anders@molloy>
%%%-------------------------------------------------------------------
-module(gen_socket_server).

-behaviour(gen_server).

%% API
-export([
	 start_link/1,
	 start_link/2,
	 start_link/3,
	 start_link/4
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("gen_socket_server.hrl").

-record(listener_state,{cbmod,cbstate,conn}).
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Port::integer()) -> {ok,Pid} | ignore | {error,Error}
%% @doc  Starts the server. Listening on port Port.
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),Port::integer()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port.
%% @end
%%--------------------------------------------------------------------
start_link(Name,Port) ->
    gen_server:start_link({local, Name}, ?MODULE, [Port,[]], []).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),Port::integer(),Opts::list()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port. Options Opts.
%% @end
%%--------------------------------------------------------------------
start_link(Name,Port,Opts) when is_list(Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Port,Opts], []).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),Port::integer(),Address::ip_address()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port on the interface with IP address Address.
%% @end
%%--------------------------------------------------------------------
start_link(Name,Port,Address) when is_tuple(Address) ->
    gen_server:start_link({local, Name}, ?MODULE, 
			  [Port,[{ip,Address}]], []).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),Port::integer(),Address::ip_address(),Opts::list()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port on the interface with IP address Address.
%% @end
%%--------------------------------------------------------------------
start_link(Name,Port,Address,Opts) when is_tuple(Address),is_list(Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, 
			  [Port,[{ip,Address}|Opts]], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc  Initiates the server.
%% @end
%%--------------------------------------------------------------------
init([Port,Opts]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, Opts) of
	{ok, Listen_socket} ->
	    %%Create first accepting process
	    Pid = start_listener(self(),Listen_socket,Port),
	    {ok, #state{listen_socket = Listen_socket,
                        port = Port,
			acceptor = Pid}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc  Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc  Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({create,_Pid},#state{listen_socket = Listen_socket} = State) ->
    New_pid = gen_socket_server_socket:start_link(self(), 
						  Listen_socket, 
						  State#state.port),
    {noreply, State#state{acceptor=New_pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc  Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
%% The current acceptor has died, wait a little and try again
%% @private
handle_info({'EXIT', Pid, _Abnormal}, #state{acceptor=Pid} = State) ->
    timer:sleep(2000),
    gen_socket_server_socket:start_link(self(), 
					State#state.listen_socket, 
					State#state.port),
    {noreply,State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc  This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

listener_start(CBMod,ListenPid, ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, listener_init, [{CBMod,
						  ListenPid, 
						  ListenSocket, 
						  ListenPort}]).

listener_init({CBMod,Listen_pid, Listen_socket, ListenPort}) ->
    case catch gen_tcp:accept(Listen_socket) of
	{ok, Socket} ->
	    %% Send the cast message to the listener process to 
	    %% create a new acceptor
	    gen_socket_server:create(Listen_pid, self()),
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #connection{sock = Socket,
			    port = ListenPort,peer_addr = Addr,
			    peer_port = Port},
	    connected(access_control(CBMod,C));
	Else ->
	    error_logger:error_report([{application, gen_socket_server},
				       "Accept failed error",
				       io_lib:format("~p",[Else])]),
	    exit({error, accept_failed})
    end.

access_control(CBMod,Conn) ->
    {allow,CBMod,Conn}.

connected({allow,CBMod,Conn}) ->
    S=CBMod:connected(LS#listener_state.connection),
    listener_recv(CBMod,C,S});
connected({deny,C}) ->
    close(C#connection.sock).

listener_recv(CBMod,C,S) ->
    case gen_tcp:recv(C#connection.sock, 0, 30000) of
	{ok,Some} ->
	    S1=CBMod:received(Some,S),
	    listener_recv(CBMod,C,S1);
	error ->
	    io:format("Got error~n",[])
    end.
