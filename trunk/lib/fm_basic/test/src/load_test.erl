%%%-------------------------------------------------------------------
%%% File    : alarm_trace.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 13 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(load_test).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
-include("CIM_AlertIndication.hrl").
-include("CIM_SNMPTrapIndication.hrl").
-include("alarm_sts.hrl").
%%--------------------------------------------------------------------
%% External exports
-export([start/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {lang,ed}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/2
%% Description: Starts the server
%%--------------------------------------------------------------------
start(N,S,M) ->
    gen_server:start(?MODULE, N, []),
    run(N,S,M).

run(N,S,M) when N>0 ->
    run(N,S,M,M).

run(N,S,M,M1) when N>0,M1>0 ->
    demo:trap(),
    run(N-1,S,M,M1-1);
run(N,S,M,0) when N>0 ->
    demo:trap(),
    timer:sleep(S),
    run(N-1,S,M,M);
run(0,S,M,M1) ->
    finished.
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
init(N) ->
    Filter=mkFilter(),
    {Pid,AList}=alarm_sts_server:subscribe({ne,'host.domain.com'},Filter),
    {ok, {0,N}}.

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
handle_info(Alarm, State={0,N}) when is_record(Alarm,as_alarm) ->
    Time=now(),
    io:format("Starting: ~w~n",[Time]),
    {noreply, {Time,N-1}};

handle_info(Alarm, {STM,1}=State) when is_record(Alarm,as_alarm) ->
    Time={EM,ES,Em}=now(),
    Dur=timer:now_diff(Time,STM)/1000000,
    io:format("Finished: ~w~n",[Time]),
    io:format("Duration: ~w~n",[Dur]),
    {stop, normal,State};

handle_info(Alarm, {STM,N}=State) when is_record(Alarm,as_alarm) ->
    {noreply, {STM,N-1}};

handle_info(Info, State) ->
    io:format("loop got: ~p~n",[Info]),
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

mkFilter() ->
    Me=self(),
    ets:fun2ms(fun (X) ->
		       {{send_to,Me},X}
	       end).
