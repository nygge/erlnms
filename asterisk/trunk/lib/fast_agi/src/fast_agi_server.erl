%%%-------------------------------------------------------------------
%%% File    : fast_agi.erl
%%% Created : 22 Feb 2006 by Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2006 Anders Nygren
%%% @version {@vsn}
%%% @doc Main process for the fast_agi server. 
%%% Spawns a listener process that listens on the assigned port.
%%% When a connection is made the listener calls fast_agi_server:create/2
%%% to create a new listener process and continues handling the request.
%%% %@private
%%%
%%%-------------------------------------------------------------------
-module(fast_agi_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER,?MODULE).
-define(PORT,4573).

-record(state, {listen_socket,
                port,
                acceptor}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Start the server.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% spec create(ServerPid, Pid) -> ok
%% @doc Create a new listener process.
%% @end
%%--------------------------------------------------------------------
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

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
%% @private
init([]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(?PORT, [list, {packet, 0},
				{reuseaddr,true},
				{active, false}]) of
	{ok, Listen_socket} ->
	    %%Create first accepting process
	    Pid = fast_agi_socket:start_link(self(), Listen_socket, ?PORT),
	    {ok, #state{listen_socket = Listen_socket,
                        port = ?PORT,
			acceptor = Pid}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% @private
handle_cast({create,_Pid},#state{listen_socket = Listen_socket} = State) ->
    New_pid = fast_agi_socket:start_link(self(), 
					 Listen_socket, 
					 State#state.port),
    {noreply, State#state{acceptor=New_pid}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% The current acceptor has died, wait a little and try again
%% @private
handle_info({'EXIT', Pid, _Abnormal}, #state{acceptor=Pid} = State) ->
    timer:sleep(2000),
    fast_agi_socket:start_link(self(), 
			       State#state.listen_socket, 
			       State#state.port),
    {noreply,State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------