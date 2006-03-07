%%%-------------------------------------------------------------------
%%% File    : pm_stored.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_stored).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("pm_rec.hrl").
-include("new_pm_data.hrl").
-include("pm_config.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER,?MODULE).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).
-export([process_data/1]).
-export([check_time/3]).

-record(state, {}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    ok=connect(),
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Data, State) when is_record(Data,pm_rec) ->
    proc_lib:spawn(?MODULE,process_data,[Data]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

connect() ->
    Pid=self(),
    Filter=ets:fun2ms(fun (X)->{{send_to,Pid},X} end),
    pm_raw_data:subscribe(Filter).

process_data(#pm_rec{moi=MOI,moc=MOC,time=Time,data=Data}) ->
    {ok,[_TS]}=pm_rrd_access:update(MOI,MOC,Time,Data),
    case pm_config:get_events(MOI,MOC) of
	{found,Events,Step} ->
	    S={Unit,No}=pm_config:get_duration(Step),
	    lists:foreach(fun (EInt) ->
				  send_event(MOI,MOC,EInt,check_time(Time,EInt,S))
			  end,Events);
	not_found ->
	    ignore
    end.

check_time(TS,EInt,Step) ->
    Secs=calendar:datetime_to_gregorian_seconds(TS),
    StepSecs=pm_config:duration_to_seconds(Step),
    ESecs=pm_config:duration_to_seconds(EInt),
    Latest=ESecs*(Secs div ESecs),
    case (Secs-Latest)<StepSecs of
	true ->
	    {true,calendar:gregorian_seconds_to_datetime(Latest)};
	false ->
	    false
    end.

send_event(MOI,MOT,EInt,{true,Time}) ->
    pm_data:send(#new_pm_data{moi=MOI,moc=MOT,int=EInt,time=Time});
send_event(_MOI,_MOT,_EInt,false) ->
    ok.
