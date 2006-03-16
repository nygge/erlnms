%%%-------------------------------------------------------------------
%%% File    : pm_store.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_store).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("pm_store.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([
%	 create/3,
% 	 fetch/1,
%%	 fetch/4,
	 fetch/5,
%% 	 fetch/6,
	 fetch/7,
% 	 graph/3,
	 update/4]).

%%====================================================================
%% External functions
%%====================================================================

%% %% @spec create(Name,MOI,Store_type,BackEnd) -> Result
%% %% Name        = atom()
%% %% MOI         = [RDN]
%% %% RDN         = {Type,Id}
%% %% Type        = atom()
%% %% Id          = atom()
%% %% Store_type  = atom()
%% %% BackEnd     = atom()
%% %% Result      = {atomic,Reply}
%% %% Reply       = WHAT
%% %% @doc Create a measurement store.

%% create(Name,MOI,Store_type,BackEnd) when is_atom(name),is_list(MOI),is_atom(Store_type)->
%%     {Step,DSS,RRA}=get_store_type_def(Store_type),
%%     [ST]=pm_config:get(pm_store_type,Store_type),
%%     MO_TYPE=ST#pm_store_type.mo_type,
%%     File=mk_file_name(Name,MOI,MO_TYPE),
%%     {ok,nothing}=rrdtool:create(File,[Step],DSS,RRA),
%%     STORE_INST=#pm_store_inst{name={MOI,MO_TYPE},store_type=Store_type,
%% 			     file=File},
%%     {atomic,Reply}=pm_config:insert(STORE_INST).

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

%% @spec fetch(MOI,MOC,Counters,CF,Res) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Counters    = [Counter]
%% Counter     = atom()
%% CF          = atom
%% Res         = Duration
%% Result      = WHAT

fetch(MOI,MOC,Counters,CF,Res) ->
    Esecs=utils:datetime_to_epoch(calendar:universal_time()),
    RSecs=utils:duration_to_seconds(Res),
    Time=utils:epoch_to_datetime((Esecs div RSecs) * RSecs),
    fetch(MOI,MOC,Counters,CF,Res,Time,Time).

% fetch(MOI,MOC,Res,Start,Stop) ->
%     {found,Counters}=pm_config:get_counters(MOI,MOC,all),
%     fetch(MOI,MOC,Counters,Res,Start,Stop).

%% @spec fetch(MOI,MOC,Counters,CF,Res,Rows,Stop) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Counters    = [Counter]
%% Counter     = atom()
%% CF          = atom
%% Res         = Duration
%% Rows        = integer()
%% Stop        = datetime()
%% Result      = WHAT

fetch(MOI,MOC,Counters,CF,Res,Rows,Stop) when is_integer(Rows),Rows>0 ->
    S=utils:datetime_to_epoch(Stop),
    D=Rows*utils:duration_to_seconds(Res),
    Start=utils:epoch_to_datetime(S-D),
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

update(MOI,MOC,Time,Data) ->
    case pm_config:get_db_backend(MOI,MOC) of
	{ok,Module} ->
	    Module:update(MOI,MOC,Time,Data);
	_ ->
	    {error,no_db_defined}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

