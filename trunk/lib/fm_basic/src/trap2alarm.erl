%%%-------------------------------------------------------------------
%%% File    : trap2alarm3.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 23 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(trap2alarm).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("trap2alarm.hrl").
-include("trap.hrl").
-include("CIM_AlertIndication.hrl").
-include("CIM_SNMPTrapIndication.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER,trap2alarm2).
%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,trap/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

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

trap(T) ->
   gen_server:cast(?SERVER,{trap,T}).
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
handle_cast({trap,Trap}, State) when is_record(Trap,trap) ->
    trap2alarm(Trap,State),
    {noreply, State};

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

%%--------------------------------------------------------------------
%% Func: trap2alarm/2
%% Purpose: Creates and sends an Alarm based on the Trap.
%% Returns: ok
%%--------------------------------------------------------------------
trap2alarm(Trap,State) ->
    Conf=getTrapConf(Trap),
    Alarm=mkAlarm(Trap,Conf,State),
    sendAlarm(Alarm).

%%--------------------------------------------------------------------
%% Func: getTrapConf/1
%% Purpose: Get the configuration data necessary for converting
%%          the Trap to an Alarm.
%% Returns: {FDN,Key,AT,SEV,PC,PCD,PRA}
%%--------------------------------------------------------------------
getTrapConf(Trap) ->
    ME=getName(Trap#trap.agentAddress),
    {Key,VB,AT,SEV,PC,PCD,PRA,FUN}=lookup(ME,Trap#trap.enterprise,
			       Trap#trap.genericTrap,
			       Trap#trap.specificTrap,
			       Trap#trap.varBindValues),
    FDN=case catch FUN(Trap) of
	    {'EXIT',_,_} ->
		ME;
	    Name ->
		Name
	end,
    {FDN,AT,SEV,PC,PCD,PRA}.

lookup(Node,Enterprise,?ENTERPRISESPECIFIC,SpecificTrap,VBValues) ->
    lookup(Node,Enterprise,{specific,SpecificTrap},VBValues);
lookup(Node,Enterprise,GenericTrap,_SpecificTrap,VBValues) ->
    lookup(Node,Enterprise,{generic,GenericTrap},VBValues).

lookup(Node,Enterprise,Trap,VBValues) ->
    MS=mkMS(Node,Enterprise,Trap,VBValues),
    Res=mnesia:dirty_select(trap2alarm,MS),
    case Res of
	[] ->
	    not_found;
	[T] when is_tuple(T) ->
	    T;
	L ->
	    Valid=filterVB(VBValues,L),
	    X=hd(lists:sort(fun (X,Y) -> X>Y end, Valid))
    end.

mkMS(Node,Enterprise,Trap,VBValues) ->
    ets:fun2ms(fun (#trap2alarm{key=Key,varbinds=VB,at=AT,sev=SEV,
				pc=PC,pcd=PCD,pra=PRA,'fun'=FUN}) when
		   ((Key=={Node,Enterprise,Trap}) or
		    (Key=={'*',Enterprise,Trap}) or
		    (Key=={'*','*',Trap}) or
		    (Key=={'*','*','*'})) ->
		       {Key,VB,AT,SEV,PC,PCD,PRA,FUN}
	       end).

filterVB(VBValues,L) ->
    lists:filter(fun ({_Key,VB,_AT,_SEV,_PC,_PCD,_PRA,_FUN}) ->
			 vbTest(VBValues,VB)
		 end, L).

vbTest(Values,[{Pos,Val}|Ms]) ->
    (Val==lists:nth(Pos,Values)) and vbTest(Values,Ms);
vbTest(Values,[]) ->
    true.

getName(Address) ->
    {ne,Address}.
    %%im:adr2me(Address).

%%--------------------------------------------------------------------
%% Func: mkAlarm/3
%% Purpose: Creates an Alarm based on the Trap.
%% Returns: Alarm
%%--------------------------------------------------------------------
mkAlarm(Trap,{FDN,AT,SEV,PC,PCD,PRA}=Conf,State) ->
    #'CIM_AlertIndication'{'IndicationTime'=calendar:local_time(),
			   'AlertingManagedElement'=FDN,
			   'AlertType'=AT,
			   'PerceivedSeverity'=SEV,
			   'ProbableCause'=PC,
			   'ProbableCauseDescription'=PCD,
			   'RecommendedActions'=PRA,
			   'Trending'=?NOTAPPLICABLE}.

sendAlarm(Alarm) ->
    raw_alarm:send(Alarm).
