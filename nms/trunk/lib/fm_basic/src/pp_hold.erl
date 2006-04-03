%%%-------------------------------------------------------------------
%%% File    : pp_hold.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 22 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_hold).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("alarm.hrl").
-include("CIM_SNMPTrapIndication.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {tab}).
-record(held,{id,alarm,tref}).

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
init(_)->
    Tab=ets:new(hold,[{keypos,2}]),
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
handle_info({hold,#'CIM_SNMPTrapIndication'{genericTrap=?LINKUP}=A,Dur},State) ->
    Key={A#'CIM_SNMPTrapIndication'.agentAddress,
	 hd(A#'CIM_SNMPTrapIndication'.varBindValues)},
    case ets:lookup(State#state.tab, Key) of
	[#held{id=Id,tref=Tref}] ->
	    timer:cancel(Tref),
	    ets:delete(State#state.tab,Key);
	[] ->
	    alarm_pp:send(A)
    end,
    {noreply, State};

handle_info({hold,A,Dur}, State) when is_record(A,'CIM_SNMPTrapIndication') ->
    Key={A#'CIM_SNMPTrapIndication'.agentAddress,
	 hd(A#'CIM_SNMPTrapIndication'.varBindValues)},
    {ok,Tref}=timer:send_after(Dur,self(),{time_out,Key}),
    ets:insert(State#state.tab, 
	       #held{id=Key,alarm=A,tref=Tref}),
    {noreply, State};

handle_info({time_out,Key}, State) ->
    case ets:lookup(State#state.tab, Key) of
	[#held{id=Id,alarm=Alarm,tref=Tref2}] ->
	    alarm_pp:send(Alarm),
	    ets:delete(State#state.tab,Key);
	[] ->
	    ignore
    end,
    {noreply, State};

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

% -module(pp_hold).

% -export([start/1,start_link/1]).
% -export([init/1]).

% -include("alarm.hrl").
% -include("CIM_SNMPTrapIndication.hrl").

% -record(state,{tab}).
% -record(held,{id,tref}).

% start(Name)->
%    Pid=proc_lib:start(?MODULE,init,[self()]).

% start_link(Name)->
%    proc_lib:start_link(?MODULE,init,[self()]).

% init(Parent)->
%    Tab=ets:new(hold,[{keypos,2}]),
%    register(?MODULE,self()), 
%    proc_lib:init_ack(Parent,{ok,self()}),
%    loop(#state{tab=Tab}).

% loop(State) ->
%     receive
% 	{time_out,Alarm1,Key1} ->
% 	    case ets:lookup(State#state.tab, Key1) of
% 		[#held{id=Id1,tref=Tref1}] ->
% 		    alarm_pp:send(Alarm1),
% 		    ets:delete(State#state.tab,Key1);
% 		[] ->
% 		    ignore
% 	    end
%     after 0 -> ok
%     end,
%     receive
% 	{hold,#'CIM_SNMPTrapIndication'{genericTrap=?LINKUP}=A,Dur} -> 
% 	    Key={A#'CIM_SNMPTrapIndication'.agentAddress,
% 		 hd(A#'CIM_SNMPTrapIndication'.varBindValues)},
% 	    case ets:lookup(State#state.tab, Key) of
% 		[#held{id=Id,tref=Tref}] ->
% 		    timer:cancel(Tref),
% 		    ets:delete(State#state.tab,Key);
% 		[] ->
% 		    alarm_pp:send(A)
% 	    end,
% 	    loop(State);
% 	{hold,A,Dur} when is_record(A,'CIM_SNMPTrapIndication') -> 
% 	    Key={A#'CIM_SNMPTrapIndication'.agentAddress,
% 		 hd(A#'CIM_SNMPTrapIndication'.varBindValues)},
% 	    Tref=timer:send_after(Dur,self(),{time_out,A,Key}),
% 	    ets:insert(State#state.tab, #held{id=Key,
% 					      tref=Tref}),
% 	    loop(State);
% 	{time_out,Alarm2,Key2} ->
% 	    case ets:lookup(State#state.tab, Key2) of
% 		[#held{id=Id2,tref=Tref2}] ->
% 		    alarm_pp:send(Alarm2),
% 		    ets:delete(State#state.tab,Key2);
% 		[] ->
% 		    ignore
% 	    end,
% 	    loop(State);
% 	Other ->
% 	    loop(State)
%     end.
