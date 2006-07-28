%%%-------------------------------------------------------------------
%%% File    : file_collector.erl
%%% Author  : Anders <anders@local>
%%% Description : 
%%%
%%% Created : 10 Jun 2004 by Anders <anders@local>
%%%-------------------------------------------------------------------
-module(file_collector).

-behaviour(gen_fsm).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([connecting/2,deleting_file/2,disconnecting/2,getting_file/2,
	 getting_file_list/2,stopping/2]).

-record(state, {worker,file=[],files=[],src,todir,tmpdir,
		connected=false,reason}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Module,Src,Dst) ->
    gen_fsm:start_link(?MODULE, [Module,Src,Dst], []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}                   
%%--------------------------------------------------------------------
init([Module,Src,Dst]) ->
    {ok,Pid}=collector_if:start_link(Module),
    ok=ensure_dir(Dst),
    Tmp=filename:join(Dst,"tmp"),
    ok=ensure_dir(filename:join(Tmp,"dummy")),
    State=#state{worker=Pid,src=Src,todir=Dst,tmpdir=Tmp},
    collector_if:connect(Pid,Src),
    {ok, connecting, State}.

%%--------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%--------------------------------------------------------------------
connecting(ok,State) ->
    collector_if:get_file_list(State#state.worker),
    {next_state,getting_file_list,State};
connecting({'EXIT',Reason},State) ->
    State1=State#state{reason=Reason},
    collector_if:terminate(State#state.worker,Reason),
    {next_state,terminating,State1}.

getting_file_list({ok,Files},State) ->
    NewState=State#state{files=Files},
    start_get_file(NewState);
getting_file_list({'EXIT',Reason},State) ->
    State1=State#state{reason=Reason},
    collector_if:terminate(State#state.worker,Reason),
    {next_state,terminating,State1}.

start_get_file(State) ->
    Files=State#state.files,
    get_file(Files,State).

get_file([F|Fs],State) ->
    Dst=filename:join(State#state.tmpdir,F),
    collector_if:get_file(State#state.worker,F,Dst),
    {next_state,getting_file,State#state{file=F,files=Fs}};
get_file([],State) ->
    State1=State#state{reason=normal},
    collector_if:disconnect(State#state.worker),
    {next_state,disconnecting,State1}.

getting_file(ok,State) ->
    File=State#state.file,
    Tmp=filename:join(State#state.tmpdir,File),
    Dst=filename:join(State#state.todir,File),
    ok=file:rename(Tmp,Dst),
    collector_if:del_file(State#state.worker,State#state.file),
    {next_state,deleting_file,State};
getting_file({'EXIT',Reason},State) ->
    State1=State#state{reason=Reason},
    collector_if:terminate(State#state.worker,Reason),
    {next_state,terminating,State1}.
			     
deleting_file(ok,State) ->
    start_get_file(State).

disconnecting(Any,State) ->
    collector_if:stop(State#state.worker),
    {next_state,stopping,State}.

stopping(Any,State) ->
    {stop,State#state.reason,State}.
    
state_name(Event, StateData) ->
    {next_state, state_name, StateData}.

%%--------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%--------------------------------------------------------------------
state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%%--------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%--------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%--------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%--------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%--------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%--------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%--------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%--------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

ensure_dir(File) ->
    case filelib:ensure_dir(File) of
	ok ->
	    ok;
	true ->
	    ok;
	Other ->
	    Other
    end.
