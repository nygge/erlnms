%%%-------------------------------------------------------------------
%%% File    : pp_action.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 22 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_action).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    Tab=ets:new(pp_action,[]),
    {ok, #state{tab=Tab}}.

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
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({act,Mod,Fun,Alarm}, State) ->
    case catch Mod:Fun(Alarm) of
	{ok,Pid} when is_pid(Pid) ->
	    ets:insert(State#state.tab,{Pid,Alarm});
	{'EXIT',Reason,Stack} ->
	    %% Insert error logging here
	    alarm_pp:send(Alarm);
	Res -> ok
    end,
    {noreply, State};

handle_info({'EXIT',Pid,normal}, State) ->
    ets:delete(State#state.tab,Pid),
    {noreply, State};

handle_info({'EXIT',Pid,Reason}, State) ->
    case ets:lookup(State#state.tab,Pid) of
	[{Pid,Alarm}] ->
	    alarm_pp:send(Alarm),
	    {noreply, State};
	[] ->
	    {stop, Reason, State}
    end;

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

