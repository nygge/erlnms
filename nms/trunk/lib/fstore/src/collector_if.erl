%%%-------------------------------------------------------------------
%%% File    : collector_if.erl
%%% Author  : Anders <anders@local>
%%% Description : 
%%%
%%% Created : 10 Jun 2004 by Anders <anders@local>
%%%-------------------------------------------------------------------
-module(collector_if).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-export([connect/2,get_file_list/1,get_file/3,
	 del_file/2,disconnect/1,stop/1]).

-record(state, {mod,s}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Mod) ->
    gen_server:start_link(?MODULE, [Mod], []).

connect(Server,Src) ->
    gen_server:cast(Server,{connect,self(),Src}).
get_file_list(Server) ->
    gen_server:cast(Server,{get_file_list,self()}).
get_file(Server,File,Dst) ->
    gen_server:cast(Server,{get_file,self(),File,Dst}).
del_file(Server,File) ->
    gen_server:cast(Server,{del_file,self(),File}).
disconnect(Server) ->
    gen_server:cast(Server,{disconnect ,self()}).
stop(Server) ->
    gen_server:cast(Server,{stop,self()}).

 %%===================================================================
 %% Server functions
 %%===================================================================

 %%--------------------------------------------------------------------
 %% Function: init/1
 %% Description: Initiates the server
 %% Returns: {ok, State}          |
 %%          {ok, State, Timeout} |
 %%          ignore               |
 %%          {stop, Reason}
 %%--------------------------------------------------------------------
 init([Mod]) ->
    case catch Mod:init() of
	{ok,State} ->
	    {ok, #state{mod=Mod,s=State}};
	{stop,Reason}=Reply ->
	    Reply;
	{'EXIT',Reason} ->
	    {stop,Reason}
    end.

 %%--------------------------------------------------------------------
 %% Function: handle_call/3
 %% Description: Handling call messages
 %% Returns: {reply, Reply, State}          |
 %%          {reply, Reply, State, Timeout} |
 %%          {noreply, State}               |
 %%          {noreply, State, Timeout}      |
 %%          {stop, Reason, Reply, State}   | (terminate/2 is called)
 %%          {stop, Reason, State}            (terminate/2 is called)
 %%--------------------------------------------------------------------
 handle_call(Request, From, State) ->
     Reply = ok,
     {reply, Reply, State}.

 %%--------------------------------------------------------------------
 %% Function: handle_cast/2
 %% Description: Handling cast messages
 %% Returns: {noreply, State}          |
 %%          {noreply, State, Timeout} |
 %%          {stop, Reason, State}            (terminate/2 is called)
 %%--------------------------------------------------------------------
handle_cast({connect,From,Src}, State) ->
    F=fun(M,S) ->
	    M:connect(Src,S)
      end,
    do_cmd(F,From,State);

handle_cast({get_file_list,From}, State) ->
    F=fun(M,S) ->
	    M:get_file_list(S)
      end,
    do_cmd(F,From,State);

handle_cast({get_file,From,File,Dst}, State) ->
    F=fun(M,S) ->
	    M:get_file(File,Dst,S)
      end,
    do_cmd(F,From,State);

handle_cast({del_file,From,File}, State) ->
    F=fun(M,S) ->
	    M:del_file(File,S)
      end,
    do_cmd(F,From,State);

handle_cast({disconnect,From}, State) ->
    F=fun(M,S) ->
	    M:disconnect(S)
      end,
    do_cmd(F,From,State);

handle_cast({stop,From}, State) ->
    send_reply(From,ok),
    {stop,normal,State};

handle_cast(Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_cmd(Fun,From,State) ->
    Mod=State#state.mod,
    MState=State#state.s,
    Res=(catch Fun(Mod,MState)),
    {Erep,Srep}=new_state(Res,State),
    send_reply(From,Erep),
    Srep.

new_state({ok,NewState},State) ->
    Erep=ok,
    NState=State#state{s=NewState},
    Srep={noreply,NState},
    {Erep,Srep};
new_state({ok,Result,NewState},State) ->
    Erep={ok,Result},
    NState=State#state{s=NewState},
    Srep={noreply,NState},
    {Erep,Srep};
new_state({error,Reason,NewState},State) ->
    Erep={error,Reason},
    NState=State#state{s=NewState},
    Srep={stop,Reason,NState},
    {Erep,Srep};
new_state({'EXIT',Reason},State) ->
    Erep={error,Reason},
    Srep={stop,Reason,State},
    {Erep,Srep}.

send_reply(To,Reply) ->
    gen_fsm:send_event(To,Reply).

