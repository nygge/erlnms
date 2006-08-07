%%%-------------------------------------------------------------------
%%% File    : pm_store.erl
%%% Created :  5 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc pm_store provides the main interface to the performance data
%%% storage. The real work is being done by the database backends.
%%% @end
%%%-------------------------------------------------------------------
-module(pm_store).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%% @headerfile "pm_store.hrl"
-include("pm_store.hrl").
-include("new_pm_data.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([create/3,
	 fetch/5,
	 fetch/7,
	 update/4]).

%%====================================================================
%% External functions
%%====================================================================

%% @spec create(MOI,Store_type,BackEnd) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% Store_type  = atom()
%% BackEnd     = atom()
%% Result      = {atomic,Reply}
%% Reply       = WHAT
%% @doc Create a measurement store.
%% @end

create(MOI,StoreType,BackEnd) when is_list(MOI),
				   is_atom(StoreType),
				   is_atom(BackEnd) ->
    ST=pm_config:get_store_def(StoreType),
    BE=pm_config:get_db_backend(BackEnd),
    MO_TYPE=(ST#pm_store_type.mo_type)#pm_mo_type.name,
    case catch (BE#pm_db_backend.module):create(MOI,ST) of
	{ok,nothing} ->
	    {atomic,Reply}=pm_config:new_store_inst(
			     #pm_store_inst{name={MOI,MO_TYPE},
					    store_type=StoreType,
					    backend=BackEnd}),
	    ok;
	Error ->
	    Error
    end.

%% %% @spec fetch(MOI,MOC,Res) -> Result
%% %% MOI         = [RDN]
%% %% RDN         = {Type,Id}
%% %% Type        = atom()
%% %% Id          = atom()
%% %% MOC         = atom()
%% %% Res         = 
%% %% Result      = WHAT

%% fetch(MOI,MOC,Res) ->
%%     {found,Counters}=pm_config:get_counters(MOI,MOC,all),
%%     fetch(MOI,MOC,Counters,Res).

%% @spec fetch(MOI,MOC,Counters,CF,Res::duration()) -> Result
%% MOI         = [RDN]
%% RDN         = {Type::atom(),Id::atom()}
%% MOC         = atom()
%% Counters    = [Counter::atom()]
%% CF          = atom
%% Result      = WHAT
%% @doc Fetch the latest row of data from the performance data store.
%% @end

fetch(MOI,MOC,Counters,CF,Res) ->
    Esecs=time:datetime_to_epoch(calendar:universal_time()),
    RSecs=time:duration_to_seconds(Res),
    Time=time:epoch_to_datetime((Esecs div RSecs) * RSecs),
    fetch(MOI,MOC,Counters,CF,Res,Time,Time).

%% @spec fetch(MOI,MOC,Counters,CF,Res,From,Stop) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Counters    = [Counter]
%% Counter     = atom()
%% CF          = atom
%% Res         = Duration
%% From        = Rows|Start
%% Rows        = integer()
%% Start       = datetime()
%% Stop        = datetime()
%% Result      = WHAT
%% @doc Fetch data from the performance data store.
%% <p>If From is an integer() fetch will return From rows of data ending
%% at time Stop.</p>
%% <p>If From is a datetime() fetch will return rows of data starting at time Start and ending at time Stop.</p>
%% @end

fetch(MOI,MOC,Counters,CF,Res,Rows,Stop) when is_integer(Rows),Rows>0 ->
    S=time:datetime_to_epoch(Stop),
    D=Rows*time:duration_to_seconds(Res),
    Start=time:epoch_to_datetime(S-D),
    fetch(MOI,MOC,Counters,CF,Res,Start,Stop);

%% @spec fetch(MOI,MOC,Counters,CF,Res,Start,Stop) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Counters    = [Counter]
%% Counter     = atom()
%% CF          = atom
%% Res         = Duration
%% Start       = datetime()
%% Stop        = datetime()
%% Result      = WHAT
%% @doc Fetch data from the performance data store.
%% @end

fetch(MOI,MOC,Counters,CF,Res,Start,Stop) when is_tuple(Start) ->
    case pm_config:get_db_backend(MOI,MOC) of
	{found,Module} ->
	    Module:fetch(MOI,MOC,Counters,CF,Res,Start,Stop);
	_ ->
	    {error,no_db_defined}
    end.

%% @spec update(MOI,MOC,Time,Data) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Time        = datetime()
%% Result      = {ok,[TimeStamp]}
%% TimeStamp   = integer()
%% @doc Insert data in the performance data store.
%% @end

update(MOI,MOC,Time,Data) ->
    case pm_config:get_db_backend(MOI,MOC) of
	{found,Module} ->
	    Module:update(MOI,MOC,Time,Data),
	    case pm_config:get_events(MOI,MOC) of
		{found,Events,Step} ->
		    S={Unit,No}=pm_config:get_duration(Step),
		    lists:foreach(fun (EInt) ->
					  send_event(MOI,MOC,EInt,check_time(Time,EInt,S))
				  end,Events);
		not_found ->
		    ignore
	    end;
	_Res ->
	    {error,no_db_defined}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
check_time(TS,EInt,Step) ->
    Secs=calendar:datetime_to_gregorian_seconds(TS),
    StepSecs=time:duration_to_seconds(Step),
    ESecs=time:duration_to_seconds(EInt),
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

