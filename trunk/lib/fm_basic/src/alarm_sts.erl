%%%-------------------------------------------------------------------
%%% File    : alarm_sts.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 12 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(alarm_sts).

-behaviour(gen_event).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
-include("CIM_AlertIndication.hrl").
-include("CIM_SNMPTrapIndication.hrl").
-include("alarm_sts.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1, start_link/2, 
	 stop/1, 
	 subscribe/2, subscribe/3,
	 unsubscribe/1, 
	 get_sts/1, ack/3, clear/3, comment/4]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {s_tab,al=[]}).
-record(sub,{pid,flt=none,mon}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(NE) ->
    {ok,Pid}=gen_event:start_link(),
    ok=gen_event:add_handler(Pid, ?MODULE, [NE]),
    {ok,Pid}.

%%--------------------------------------------------------------------
%% Function: start_link/2
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name,NE) ->
    {ok,Pid}=gen_event:start_link(Name),
    ok=gen_event:add_handler(Pid, ?MODULE, [NE]),
    {ok,Pid}.

%%--------------------------------------------------------------------
%% Function: stop/1
%% Description: Stop the server
%%--------------------------------------------------------------------
stop(Name) ->
    gen_event:stop(Name).

%%--------------------------------------------------------------------
%% Function: subscribe/2
%% Description: Adds an event handler
%% EventMgr = Registered name | pid of event manager
%% Filter = match specification, (see ets:fun2ms/1), that returns
%%          {Fun,term} | {{send_to,Pid},term}
%%--------------------------------------------------------------------
subscribe(EventMgr,Filter) -> 
    subscribe(EventMgr,self(),Filter).

subscribe(EventMgr,From,[]) -> 
    Filter=[{'$1',[],[{{{{send_to,{const,From}}},'$1'}}]}],
    subscribe(EventMgr,From,Filter);

subscribe(EventMgr,From,Filter) -> 
    case ets:test_ms({},Filter) of
	{ok,Res} -> 
 	    AList=gen_event:call(EventMgr,?MODULE,{subscribe,From,Filter}),
	    {EventMgr,AList};
	Error -> Error
    end.
%%gen_event:add_sup_handler(EventMgr, {subscription_event_h,self()},Filter),
%%{EventMgr,get_sts(EventMgr)}.

%%--------------------------------------------------------------------
%% Function: unsubscribe/1
%% Description: Remove an event handler
%% EventMgr = Registered name | pid of event manager
%%--------------------------------------------------------------------
unsubscribe(EventMgr) -> 
    gen_server:call(EventMgr,{unsubscribe,self()}).
    %gen_event:delete_handler(EventMgr, {subscription_event_h,self()},[]).

%%--------------------------------------------------------------------
%% Function: get_sts/1
%% Description: Get the current alarm status
%%--------------------------------------------------------------------
get_sts(EventMgr) ->
   gen_event:call(EventMgr,?MODULE,get_sts).

%%--------------------------------------------------------------------
%% Function: ack/3
%% Description: Acknowledge an alarm
%%--------------------------------------------------------------------
ack(EventMgr,AlarmId,Who) ->
   gen_event:call(EventMgr,?MODULE,{ack,AlarmId,Who}).

%%--------------------------------------------------------------------
%% Function: clear/3
%% Description: Clear an alarm
%%--------------------------------------------------------------------
clear(EventMgr,AlarmId,Who) ->
   gen_event:call(EventMgr,?MODULE,{clear,AlarmId,Who}).

%%--------------------------------------------------------------------
%% Function: comment/4
%% Description: Add a comment to an alarm
%%--------------------------------------------------------------------
comment(EventMgr,AlarmId,Comment,Who) ->
   gen_event:call(EventMgr,?MODULE,{comment,AlarmId,Comment,Who}).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%--------------------------------------------------------------------
init([{Class,NE}=ME]) ->
    process_flag(trap_exit, true),
    S_tab=ets:new(subs,[ordered_set,{keypos,2}]),
    Filter=mkFilter(ME),
    alarm_pp:subscribe(Filter),
    AList=ets:new(NE,[{keypos,2}]),
    {ok, #state{s_tab=S_tab,al=AList}}.

%%--------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_event(A, State) when is_record(A, 'CIM_AlertIndication') ->
    case A#'CIM_AlertIndication'.'PerceivedSeverity' of
	?CEASE ->
	    update_status(cease,A,State);
	Other ->
	    update_status(new,A,State)
    end,
    {ok, State};

handle_event(A, State) when is_record(A, 'CIM_ThresholdIndication') ->
    CIM_AI=A#'CIM_ThresholdIndication'.'CIM_AI',
    case CIM_AI#'CIM_AlertIndication'.'PerceivedSeverity' of
	?CEASE ->
	    update_status(cease,A,State);
	Other ->
	    update_status(new,A,State)
    end,
    {ok, State};

handle_event(A, State) when is_record(A, 'CIM_SNMPTrapIndication') ->
    send_events(A,State),
    {ok, State};

handle_event(E,State) ->
    {ok,State}.

%%--------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%--------------------------------------------------------------------
handle_call({subscribe,Pid,Filter},State)->
    Res=subscribe1(Pid,Filter,State),
    AList=al_get_all(State#state.al),
    {ok,AList,State};

handle_call({unsubscribe,Pid},State)->
    Res=unsubscribe(Pid,State),
    {ok,Res,State};

handle_call(get_sts, State) ->
    AList=al_get_all(State#state.al),
    Reply = AList,
    {ok, Reply, State};

handle_call({ack, Id, Who}, State) ->
    update_status(ack,Id,Who,State), 
    Reply = ok,
    {ok, Reply, State};

handle_call({clear, Id, Who}, State) ->
    update_status(clear,Id,Who,State),
    Reply = ok,
    {ok, Reply, State};

handle_call({comment, Id, Comment, Who}, State) ->
    update_status(comment,Id,Who,Comment,State),
    Reply = ok,
    {ok, Reply, State};

handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_info({'DOWN',MRef,process,Pid,Reason}=E,State)->
   unsubscribe(Pid,State),
   {ok,State};

handle_info({'EXIT',Pid,Reason}=E,State)->
   unsubscribe(Pid,State),
   {ok,State};

handle_info({ack, Id, Who}, State) ->
    update_status(ack,Id,Who,State), 
    {ok, State};

handle_info({clear, Id, Who}, State) ->
    update_status(clear,Id,Who,State),
    {ok, State};

handle_info({comment, Id, Comment, Who}, State) ->
    update_status(comment,Id,Who,Comment,State),
    {ok, State};

handle_info(Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
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

subscribe1(Pid,Filter,S) ->
   case ets:lookup(S#state.s_tab,Pid) of
      [] ->
%%         Mon=erlang:monitor(process,Pid),
	 Mon=[],
         link(Pid),
         Sub=#sub{pid=Pid,flt=Filter,mon=Mon},
         ets:insert(S#state.s_tab,Sub);
      X when record(X,sub) ->
         {error,already_subscribed}
   end.

unsubscribe(Pid,State) ->
   case ets:lookup(State#state.s_tab,Pid) of
      [#sub{pid=Pid,mon=MonRef}] ->
         ets:delete(State#state.s_tab,Pid);
%         erlang:demonitor(MonRef);
      [] -> 
         {error,not_subscribed}
   end.

mkFilter(Name) ->
    Me=self(),
    ToMe=fun(X) -> gen_event:notify(Me,X) end,
    {C,Host}=Name,
    ets:fun2ms(fun (X) 
		   when is_record(X,'CIM_AlertIndication') 
		   and (hd(X#'CIM_AlertIndication'.'AlertingManagedElement')==Name) ->
		       {ToMe,X};
		   (X) 
		   when is_record(X,'CIM_ThresholdIndication') 
		   and (hd((X#'CIM_ThresholdIndication'.'CIM_AI')#'CIM_AlertIndication'.'AlertingManagedElement')==Name) ->
		       {ToMe,X};
		   (X) 
		   when is_record(X,'CIM_SNMPTrapIndication') 
		   and (X#'CIM_SNMPTrapIndication'.agentAddress==Host) -> {ToMe,X}
	       end).

update_status(new,Alarm,State) when is_record(Alarm,'CIM_AlertIndication') ->
    Key=mkKey(Alarm),
    A=#as_alarm{id=Key,alarm=Alarm},
    send_events(A,State),
    al_add(Key,A,State#state.al);
update_status(new,Alarm,State) when is_record(Alarm,'CIM_ThresholdIndication')->
    Key=mkKey(Alarm#'CIM_ThresholdIndication'.'CIM_AI'),
    A=#as_alarm{id=Key,alarm=Alarm},
    send_events(A,State),
    al_add(Key,A,State#state.al);
% update_status(new,Alarm,State) when is_record(Alarm,'CIM_SNMPTrapIndication')->
%     ok;

update_status(cease,Alarm,State) when is_record(Alarm,'CIM_AlertIndication') ->
    Id=mkKey(Alarm),
    A=#as_cease{id=Id,time=calendar:local_time()},
    send_events(A,State),
    al_del(Id,State#state.al);

update_status(cease,Alarm,State) when is_record(Alarm,'CIM_ThresholdIndication')->
    Id=mkKey(Alarm#'CIM_ThresholdIndication'.'CIM_AI'),
    A=#as_cease{id=Id,time=calendar:local_time()},
    send_events(A,State),
    al_del(Id,State#state.al).

update_status(ack,Id,Who,State) ->
    case al_ack(Id,Who,State#state.al) of
	true ->
	    send_events(#as_ack{id=Id,who=Who,
				time=calendar:local_time()},
			State);
	false ->
	    ok
    end;

update_status(clear,Id,Who,State) ->
    case al_clear(Id,State#state.al) of
	true ->
	    send_events(#as_clear{id=Id,who=Who,
				  time=calendar:local_time()},
			State);
	false ->
	    ok
    end.

update_status(comment,Id,Comment,Who,State) ->
    al_comment(Id,Comment,Who,State#state.al).

mkKey(E) ->
    {getME(E#'CIM_AlertIndication'.'AlertingManagedElement'),
     E#'CIM_AlertIndication'.'IndicationIdentifier',
     E#'CIM_AlertIndication'.'AlertType',
     E#'CIM_AlertIndication'.'ProbableCause'}.
getME([ME]) ->
    ME;
getME([NE|More]) ->
    More.

al_get_all(AList) ->
    ets:tab2list(AList).
al_add(Id,Alarm,AList) ->
    ets:insert(AList,Alarm).
al_del(Id,AList) ->
    ets:delete(AList,Id).
al_ack(Id,Who,AList) ->
    case ets:lookup(AList,Id) of
	[Row] ->
	    %% Add sending ack to alarm log
	    ets:insert(AList,
		       Row#as_alarm{ack=#ack{who=Who,
					     time=calendar:local_time()}});
	[] -> 
	    false
    end.

al_clear(Id,AList) ->
    case ets:lookup(AList,Id) of
	[Row] ->
	    %% Add sending clear to alarm log
	    ets:delete(AList,Id);
	[] -> 
	    false
    end.


    % Add sending clear to alarm log
al_comment(Id,Comment,Who,AList) ->
    [Row]=ets:lookup(AList,Id),
    % Add sending comment to alarm log
    Cs=Row#as_alarm.comments,
    ets:insert(AList,Row#as_alarm{comments=[{Who,calendar:local_time(),Comment}
					    |Cs]}).
    

send_events(Event,State) ->
    Subs=ets:tab2list(State#state.s_tab),
    lists:foreach(fun (S)-> 
			 filter_and_send(S#sub.flt,Event)
                 end, Subs).

filter_and_send(Filter,Event) ->
    filter(ets:test_ms(Event,Filter)).

filter({ok,false})->
    ok;
filter({ok,Something})->
   send(Something).

send({{send_to,Pid},Event})->
   Pid!Event;
send({F,Event}) when is_function(F) ->
   F(Event).

%trace(Dev,Event,Name) ->
%   io:format(Dev,"~p event = ~p~n",[Name,Event]).

