%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : gen_socket_server.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Generic socket server behaviour.
%%% @end 
%%% Created :  2 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_socket_server).

-behaviour(gen_server).

%% API
-export([
	 start_link/6,
	 start_link/3,
	 start_link/4,
	 start_link/6,
	 get_adm_state/1,set_adm_state/2,
	 get_op_state/1,
	 get_stats/1,
	 send/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% gen_socket_server internal callbacks
-export([listener_init/1]).

%-export([behavior_info/1]).

-include("gen_socket_server.hrl").

-record(listener_state,{cbmod,cbstate,conn}).
-record(state, {cbmod,
		cbpars,
		adm_state,
		op_state,
		op_state_reason,
		port,
		opts,
		sock,
		listener,
		connections=[],
		cnt_total=0,
		cnt_normal=0,
		cnt_failed=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(CBMod::atom(),CBPars::term(),Port::integer()) -> {ok,Pid} | ignore | {error,Error}
%% @doc  Starts the server. Listening on port Port.
%% @end
%%--------------------------------------------------------------------
start_link(CBMod,CBPars,Port) ->
    start_link(?MODULE, CBMod, CBPars, Port).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),CBMod::atom(),CBPars::term(),Port::integer()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port.
%% @end
%%--------------------------------------------------------------------
start_link(Name,CBMod,CBPars,Port) ->
    start_link(Name,CBMod,CBPars,Port,[]).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),CBMod::atom(),CBPars::term(),Port::integer(),Opts::list()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port. Options Opts.
%% @end
%%--------------------------------------------------------------------
start_link(Name,CBMod,CBPars,Port,Opts) when is_list(Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [CBMod,CBPars,Port,Opts], []);

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),CBMod::atom(),CBPars::term(),Port::integer(),Address::ip_address()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port on the interface with IP address Address.
%% @end
%%--------------------------------------------------------------------
start_link(Name,CBMod,CBPars,Port,Address) when is_tuple(Address) ->
    gen_server:start_link({local, Name}, ?MODULE, 
			  [CBMod,CBPars,Port,[{ip,Address}]], []).

%%--------------------------------------------------------------------
%% @spec start_link(Name::atom(),CBMod::atom(),CBPars::term(),Port::integer(),Address::ip_address(),Opts::list()) -> {ok,Pid} | ignore | 
%%                                                   {error,Error}
%% @doc  Starts the server. 
%% The server will register with name {local,Name}.
%% Listening on port Port on the interface with IP address Address.
%% @end
%%--------------------------------------------------------------------
start_link(Name,CBMod,CBPars,Port,Address,Opts) when is_tuple(Address),
						     is_list(Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, 
			  [CBMod,CBPars,Port,[{ip,Address}|Opts]], []).

get_adm_state(Server) ->
    gen_server:call(Server,get_adm_state).

set_adm_state(Server,State) when State==enabled;State==disabled ->
    gen_server:call(Server,{set_adm_state,State}).

get_op_state(Server) ->
    gen_server:call(Server,get_op_state).

get_stats(Server) ->
    gen_server:call(Server,get_stats).

%% @spec send(Server,Data) -> ok | {error,Reason}
%% @doc Send message to connection client. This function should only be used by 
%% a callback module to send a message to its peer.
%% @end
send(Conn,Data) ->
    gen_tcp:send(Conn#connection.sock,Data).

%%====================================================================
%% behaviour callbacks
%%====================================================================
behaviour_info(callbacks) ->
    [{init,2},{handle_data,2},{handle_closed,1},
     {handle_info,2},{handle_error,2},{terminate,2}];
behaviour_info(_Other) ->
    undefined.

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
init([CBMod,CBPars,Port,Opts]) ->
    process_flag(trap_exit, true),
    S1=#state{adm_state=enabled,
	      cbmod=CBMod,
	      cbpars=CBPars,
	      port = Port,
	      opts=Opts},
    S2=case open_listen_socket(S1) of
	   {success,State} ->
	       start_link_listener(State);
	   {failure,State} ->
	       State
       end,
    {ok,S2}.

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
handle_call(get_adm_state, _From, State) ->
    Reply = State#state.adm_state,
    {reply, Reply, State};

handle_call({set_adm_state,NewState}, _From, State) 
  when State#state.adm_state==NewState ->
    {reply, ok, State};

handle_call({set_adm_state,enabled}, _From, State) ->
    case open_listen_socket(State) of
	{success,S1} ->
	    S2=start_link_listener(S1),
	    S3=S2#state{adm_state=enabled},
	    {reply, ok, S3};
	{failure,S1} ->
	    {reply, ok, S1}
    end;

handle_call({set_adm_state,disabled}, _From, State) ->
    S1=close_listen_socket(State),
    S2=close_connections(S1),
    S3=S2#state{adm_state=disabled},
    {reply, ok, S3};

handle_call(get_op_state, _From, State) ->
    case State#state.op_state of
	up ->
	    {reply, up, State};
	down ->
	    {reply, {down,State#state.op_state_reason}, State}
    end;

handle_call(get_stats, _From, State) ->
    {reply,{State#state.cnt_total,
	    State#state.cnt_normal,
	    State#state.cnt_failed},
     State};

handle_call(_Req, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc  Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc  Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
%% The current listener has died, wait a little and try again
%% @private
handle_info({connected,Pid}, State) ->
    S1=start_link_listener(State),
    Cons=S1#state.connections,
    Cnt=S1#state.cnt_total,
    {noreply, S1#state{connections=[Pid|Cons],cnt_total=Cnt+1}};

handle_info({'EXIT', Pid, Reason}, #state{listener=Pid} = State) ->
    case State#state.adm_state of
	enabled ->
	    timer:send_after(2000,start_listener),
	    {noreply,State#state{op_state=down,
				 op_state_reason=Reason,
				 sock=undefined,
				 listener=undefined}};
	disabled ->
	    {noreply,State#state{listener=undefined,sock=undefined}}
    end;

handle_info({'EXIT', Pid, normal}, State) ->
    Conns = State#state.connections,
    case lists:member(Pid,Conns) of
	true ->
	    Cnt=State#state.cnt_normal,
	    S1=State#state{connections=lists:delete(Pid,Conns),cnt_normal=Cnt+1},
	    {noreply,S1};
	false ->
	    {noreply,State}
    end;

handle_info({'EXIT', Pid, Reason}, State) ->
    Conns = State#state.connections,
    case lists:member(Pid,Conns) of
	true ->
	    Cnt=State#state.cnt_failed,
	    S1=State#state{connections=lists:delete(Pid,Conns),cnt_failed=Cnt+1},
	    {noreply,S1};
	false ->
	    {stop,Reason,State}
    end;

handle_info(start_listener, State) ->
    case open_listen_socket(State) of
	{success,S1} ->
	    S2=start_link_listener(S1),
	    {noreply, S2};
	{failure,State} ->
	    timer:send_after(2000,start_listener),
	    {noreply, State}
    end;

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
%% Internal functions
%%--------------------------------------------------------------------
open_listen_socket(State) ->
    case gen_tcp:listen(State#state.port,State#state.opts) of
	{ok,Socket} ->
	    {success,State#state{sock=Socket,op_state=up}};
	{error,Reason} ->
	    {failure,State#state{op_state=down,op_state_reason=Reason}}
    end.

close_listen_socket(State) ->
    Listener=State#state.listener,
    exit(Listener,stoppped),
    gen_tcp:close(State#state.sock),
    State#state{op_state=down,op_state_reason=adm_state}.

close_connections(State) ->
    Conns=State#state.connections,
    lists:foreach(fun (Pid) ->
			  Pid!{gss,terminate,closed}
		  end,Conns),
    State#state{connections=[]}.

start_link_listener(State) ->
    Pid=proc_lib:spawn_link(?MODULE, listener_init, 
			    [{State#state.cbmod, State#state.cbpars, 
			      self(), State#state.sock}]),
    State#state{listener=Pid,op_state=up}.

listener_init({CBMod,CBPars, Pid, LSocket}) ->
    case catch gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    %% Send the cast message to the listener process to 
	    %% create a new acceptor
	    Pid!{connected,self()},
	    {ok, {Addr, Port}} = inet:peername(Socket),
            C = #connection{sock = Socket,
			    peer_addr = Addr,
			    peer_port = Port},
	    connected(access_control(CBMod,CBPars,C));
	{error,Reason} ->
	    error_logger:error_report([{application, gen_socket_server},
				       "Accept failed error",
				       io_lib:format("~p",[Reason])]),
	    exit({error, accept_failed, Reason})
    end.

%% @todo Do something intelligent here.
access_control(CBMod,CBPars,Conn) ->
    {allow,CBMod,CBPars,Conn}.

connected({allow,CBMod,CBPars,Conn}) ->
    {ok,S}=do_callback(CBMod,init,[CBPars,Conn]),
    listen_loop(CBMod,Conn,S,[]);

connected({deny,Conn}) ->
    gen_tcp:close(Conn#connection.sock).

listen_loop(CBMod,Conn,S,Cont) ->
    receive
	{tcp,_Socket,Data} ->
	    case do_callback(CBMod,handle_data,[Data++Cont],S) of
		{ok,Cont1,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont1);
		{stop,Reason} ->
		    exit(Reason)
	    end;
	{tcp_closed,_Socket} ->
	    case do_callback(CBMod,handle_closed,[],S) of
		{stop,Reason} ->
		    exit(Reason);
		{ok,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont)
	    end;
	{tcp_error,_Socket,Reason} ->
	    case do_callback(CBMod,handle_error,[Reason],S) of
		{ok,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont);
		{stop,Reason} ->
		    exit(Reason)
	    end;
	{gss,terminate,Reason} ->
	    do_callback(CBMod,terminate,[Reason],S),
	    exit(Reason);
	Info ->
	    case do_callback(CBMod,handle_info,[Info],S) of
		{ok,S1} ->
		    listen_loop(CBMod,Conn,S1,Cont);
		{stop,Reason} ->
		    exit(Reason)
	    end
    end.

do_callback(Mod,Fun,Pars) ->
    do_callback1(Mod,Fun,Pars).

do_callback(Mod,Fun,Pars,State) ->
    do_callback1(Mod,Fun,Pars++[State]).

do_callback1(Mod,Fun,Pars) ->
    apply(Mod,Fun,Pars).
