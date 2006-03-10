%%  Action           Privilege        Synopsis
%%   ------           ---------        --------
%%   AbsoluteTimeout  call,all         Set Absolute Timeout
%%   AgentCallbackLo  agent,all        Sets an agent as logged in by callback
%%   AgentLogoff      agent,all        Sets an agent as no longer logged in
%%   Agents           agent,all        Lists agents and their status
%%   ChangeMonitor    call,all         Change monitoring filename of a channel
%%   Command          command,all      Execute Asterisk CLI Command
%%   DBGet            system,all       Get DB Entry
%%   DBPut            system,all       Put DB Entry
%%   Events           <none>           Control Event Flow
%%   ExtensionState   call,all         Check Extension Status
%%   Getvar           call,all         Gets a Channel Variable
%%   Hangup           call,all         Hangup Channel
%%   IAXnetstats      <none>           Show IAX Netstats
%%   IAXpeers         <none>           List IAX Peers
%%   ListCommands     <none>           List available manager commands
%%   Logoff           <none>           Logoff Manager
%%   MailboxCount     call,all         Check Mailbox Message Count
%%   MailboxStatus    call,all         Check Mailbox
%%   Monitor          call,all         Monitor a channel
%%   Originate        call,all         Originate Call
%%   ParkedCalls      <none>           List parked calls
%%   Ping             <none>           Keepalive command
%%   QueueAdd         agent,all        Add interface to queue.
%%   QueuePause       agent,all        Makes a queue member temporarily unavailable
%%   QueueRemove      agent,all        Remove interface from queue.
%%   Queues           <none>           Queues
%%   QueueStatus      <none>           Queue Status
%%   Redirect         call,all         Redirect (transfer) a call
%%   SetCDRUserField  call,all         Set the CDR UserField
%%   Setvar           call,all         Set Channel Variable
%%   SIPpeers         system,all       List SIP peers (text format)
%%   SIPshowpeer      system,all       Show SIP peer (text format)
%%   Status           call,all         Lists channel status
%%   StopMonitor      call,all         Stop monitoring a channel
%%   ZapDialOffhook   <none>           Dial over Zap channel while offhook
%%   ZapDNDoff        <none>           Toggle Zap channel Do Not Disturb status OFF
%%   ZapDNDon         <none>           Toggle Zap channel Do Not Disturb status ON
%%   ZapHangup        <none>           Hangup Zap Channel
%%   ZapShowChannels  <none>           Show status zapata channels
%%   ZapTransfer      <none>           Transfer Zap Channel



%%%-------------------------------------------------------------------
%%% File    : ast_manager.erl
%%% Author  : anders <anders@>
%%% Description : 
%%%
%%% Created : 20 Feb 2006 by anders <anders@>
%%%-------------------------------------------------------------------
-module(ast_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
%% 	 absolute_timeout
%% 	 agent_callback_login
%% 	 agent_logoff
%% 	 agents
%% 	 change_monitor
%% 	 command
%% 	 db_get
%% 	 db_put
%% 	 events
%% 	 extension_state
%% 	 get_var
%% 	 hangup
%% 	 iax_netstats
%% 	 iax_peers
%% 	 list_commands
%% 	 logoff
	 mailboxcount/1,
%% 	 mailbox_status
%% 	 monitor
%% 	 originate
%% 	 parked_calls
 	 ping/0,
%% 	 queue_add
%% 	 queue_pause
%% 	 queue_remove
%% 	 queues
%% 	 queue_status
%% 	 redirect
%% 	 set_cdr_user_field
%% 	 set_var
	 sip_peers/0,
	 sip_showpeer/1,
	 status/0
%% 	 stop_monitor
%% 	 zap_dial_offhook
%% 	 zap_dnd_off
%% 	 zap_dnd_on
%% 	 zap_hangup
%% 	 zap_show_channels
%% 	 zap_transfer
	]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER,?MODULE).

-record(state, {sock,rest=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% 	 absolute_timeout(Channel,Timeout)
%% 	 agent_callback_login(Agent,Exten)
%% 	 agent_callback_login(Agent,Exten,Context,AckCall,WrapUpTime)
%% 	 agent_logoff(Agent)
%% 	 agent_logoff(Agent,Soft)
%% 	 agents()
%% 	 change_monitor(Channel,File)
%% 	 command(Command)
%% 	 db_get ????
%% 	 db_put ???
%% 	 events(EventMask)
%% 	 extension_state(Extension,Context)
%% 	 get_var(Channel,Var)
%% 	 hangup(Channel)
%% 	 iax_netstats()
%% 	 iax_peers()
%% 	 list_commands()
%% 	 logoff()

mailboxcount(Mbox) ->
    gen_server:call(?SERVER,{mailboxcount,Mbox}).

%% 	 mailbox_status(Mailbox)
%% 	 monitor(Channel,File,Format,Mix)
%% 	 originate(Channel,......)
%% 	 parked_calls()

ping() ->
    gen_server:call(?SERVER,ping).

%% 	 queue_add ????
%% 	 queue_pause(Queue ???)
%% 	 queue_remove ???
%% 	 queues()
%% 	 queue_status(Queue)
%% 	 redirect(Channel,Exten,Context,Priority)
%% 	 set_cdr_user_field ???
%% 	 set_var(Channel,Variable,Value)

sip_showpeer(Peer) ->
    gen_server:call(?SERVER,{sip_showpeer,Peer}).

sip_peers() ->
    gen_server:call(?SERVER,sip_peers).

status() ->
    gen_server:call(?SERVER,status).

%% 	 stop_monitor
%% 	 zap_dial_offhook
%% 	 zap_dnd_off
%% 	 zap_dnd_on
%% 	 zap_hangup
%% 	 zap_show_channels
%% 	 zap_transfer

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok,Sock} = gen_tcp:connect({192,168,1,50},5038,[list]),
    login(Sock),
    {ok, #state{sock=Sock}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({mailboxcount,Mbox}, _From, State) ->
    Reply=mailboxcount(State#state.sock,Mbox),
    {reply, Reply, State};
handle_call(ping, _From, State) ->
    Reply=ping(State#state.sock),
    {reply, Reply, State};
handle_call({sip_showpeer,Peer}, _From, State) ->
    Reply=sip_showpeer(State#state.sock,Peer),
    {reply, Reply, State};
handle_call(sip_peers, _From, State) ->
    Reply=sip_peers(State#state.sock),
    {reply, Reply, State};
handle_call(status, _From, State) ->
    Reply=status(State#state.sock),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp,_Sock,"Asterisk Call Manager"++_More}, State) ->
    {noreply, State};
handle_info({tcp,_Sock,Data}, State) ->
    {Msgs,Rest}=parse(State#state.rest ++ Data),
    lists:foreach(fun (Msg) ->
			  io:format("Got ~p~n",[Msg])
		  end,Msgs),
    {noreply, State#state{rest=Rest}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
login(Sock) ->
    Pack="Action: login\r\n"
	"Username: anders\r\n"
	"Secret: secret\r\n",
    ast_man_drv:send(Sock,Pack).
    
mailboxcount(Sock,Mbox) ->
    Pack=["Action: MailboxCount\r\n",
	  "Mailbox: ",Mbox,"\r\n"],
    ast_man_drv:send(Sock,Pack).

ping(Sock) ->
    Pack="Action: Ping\r\n",
    ast_man_drv:send(Sock,Pack).
    
sip_showpeer(Sock,Peer) ->
    Pack=["Action: SIPshowpeer\r\n",
	  "Peer: ",Peer,"\r\n"],
    ast_man_drv:send(Sock,Pack).
    
sip_peers(Sock) ->
    Pack="Action: SIPpeers\r\n",
    ast_man_drv:send(Sock,Pack).
    
status(Sock) ->
    Pack="Action: Status\r\n",
    ast_man_drv:send(Sock,Pack).
    
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
send(Sock,Pack) ->
    io:format("sending ~p~n",[Pack]),
    case gen_tcp:send(Sock,Pack) of
 	ok ->
 	    ok;
 	{error,Error} ->
 	    Error
    end.

parse(Data) ->
    parse_it(Data,[]).

parse_it(Data,Acc) ->
    case split(Data) of
	{nothing,Rest} ->
	    {lists:reverse(Acc),Rest};
	{Msg,More} ->
	    Msg1=msg_type(Msg),
	    parse_it(More,[Msg1|Acc])
    end.

split(Data) ->
    case string:str(Data,"\r\n\r\n") of
	Pos when Pos>0 ->
	    Msg=string:substr(Data,1,Pos-1),
	    Msg1=string:tokens(Msg,"\r\n"),
	    Msg2=[split_line(L)|| L<- Msg1],
	    More=string:substr(Data,Pos+4),
	    {Msg2,More};
	_NotFound ->
	    {nothing,Data}
    end.
%%

split_line(L) ->
    Pos=string:str(L,": "),
    Lbl=string:substr(L,1,Pos-1),
    Value=string:substr(L,Pos+2),
    {Lbl,Value}.
    
msg_type([{"Event",_EventType}|_More]=Msg) ->
    {event,Msg};
msg_type([{"Response",_EventType}|_More]=Msg) ->
    {response,Msg}.
