%%%-------------------------------------------------------------------
%%% File    : alarm_sts_server.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 12 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(alarm_sts_server).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
-define(SERVER,?MODULE).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1,get_pid/1, subscribe/2, unsubscribe/1,get_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {me}).
-record(obj_cre_evt, {class, id, sts, addinfo=[]}).
-record(obj_del_evt, {class, id, addinfo=[]}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: get_pid/1
%% Description: Get the pid of the alarm_sts that handles ME
%% Returns: {ok,Pid} | error
%%--------------------------------------------------------------------
get_pid(ME) ->
    gen_server:call(?SERVER,{get_pid,ME}).

%%--------------------------------------------------------------------
%% Function: subscribe/1
%% Description: Start a subscription on alarms from ME
%% ME = Name of a Managed Element
%%--------------------------------------------------------------------
subscribe(ME) ->
    subscribe(ME,[]).

%%--------------------------------------------------------------------
%% Function: subscribe/2
%% Description: Start a subscription on alarms from ME
%% ME = Name of a Managed Element
%% Filter = A match specification that decides which alarms to 
%%          subscribe.
%%--------------------------------------------------------------------
subscribe(ME,Filter) ->
    case get_pid(ME) of
	{ok,Pid} ->
	    alarm_sts:subscribe(Pid,Filter);
	_ ->
	    {error,no_such_me}
    end.

%%--------------------------------------------------------------------
%% Function: unsubscribe/1
%% Description: Stop a subscription on alarms from ME
%% ME = Name of a Managed Element
%%--------------------------------------------------------------------
unsubscribe(ME) ->
    {value,Pid}=get_pid(ME),
    alarm_sts:unsubscribe(Pid).

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
    process_flag(trap_exit, true),
    Tab=ets:new(as_me,[set,{keypos,1}]),
    Filter=mkFilter(),
%%    obj_mgr:subscribe(Filter),
    {ok, #state{me=Tab}}.

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
handle_call({get_pid,ME}, From, State) ->
    Reply = get_pid(ME,State#state.me),
    {reply, Reply, State};

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
handle_info(Event, State) when is_record(Event,obj_cre_evt) ->
    new_ME(Event#obj_cre_evt.id,State#state.me),
    {noreply, State};

handle_info(Event, State) when is_record(Event,obj_del_evt) ->
    del_ME(Event#obj_del_evt.id,State#state.me,del),
    {noreply, State};

handle_info({subscribe,ME,From},State) ->
    {ok,Pid}=get_pid(ME,State#state.me),
    Resp=alarm_sts:subscribe(Pid,From,[]),
    From!{subscribe_resp,Resp},
    {noreply, State};

handle_info({'EXIT', Pid, Reason} , State) ->
    del_ME(Pid,State#state.me,'EXIT'),
    {noreply, State};

handle_info({gen_event_EXIT, Pid, Reason} , State) ->
    % alarm_pp has died, wait for it to restart then resubscribe
    % What shall be done with all alarm_sts?
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
%% Internal functions
%%--------------------------------------------------------------------

get_pid(ME,Tab) ->
    case catch ets:lookup_element(Tab,ME,2) of
	Pid when is_pid(Pid) ->
	    {ok,Pid};
	{'EXIT',_} ->
	    false
    end.
 
new_ME([ME],Tab) ->
    {ok,Pid}=alarm_sts:start_link(ME),
    ets:insert(Tab,{ME,Pid}).

del_ME(ME,Tab,'EXIT') ->
    {ok,Pid}=ets:delete(Tab,ME),
    catch alarm_sts:stop(Pid);

del_ME(ME,Tab,del) ->
    Pid=ets:lookup_element(Tab,ME,2),
    ets:delete(Tab,ME),
    catch alarm_sts:stop(Pid).

mkFilter() ->
    ToMe={send_to,self()},
    ets:fun2ms(fun (X) when is_record(X,obj_cre_evt) and (X#obj_cre_evt.class==me) ->
		       {ToMe,X};
		   (X) when is_record(X,obj_del_evt) and (X#obj_del_evt.class==me) ->
		       {ToMe,X}
	       end).
