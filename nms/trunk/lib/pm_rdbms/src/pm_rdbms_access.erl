%%%-------------------------------------------------------------------
%%% File    : pm_rdbms_access.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_rdbms_access).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("pm_rec.hrl").
-include("pm_config.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([create/3,
	 fetch/7,
	 update/4]).

%%====================================================================
%% External functions
%%====================================================================

%% @spec create(Name,MOI,Store_type) -> Result
%% Name        = atom()
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% Store_type  = atom()
%% Result      = {atomic,Reply}
%% Reply       = WHAT
%% @doc Create a measurement store.

create(Name,MOI,Store_type) when is_atom(name),is_list(MOI),is_atom(Store_type)->
    ok.

%% @spec fetch(MOI,MOC,Counters,CF,Res,Start,Stop) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Counters    = [Counter]
%% Counter     = atom()
%% Res         = 
%% Start       = datetime()
%% Stop        = datetime()
%% Result      = WHAT

fetch(MOI,MOC,Counters,CF,Res,Start,Stop) when is_tuple(Start) ->
    not_implemented.

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
    {found,File}=pm_config:id_to_file(MOI,MOC),
    {found,Counters}=pm_config:get_counters(MOI,MOC,primary),
    TS1=mk_ts(Time),
    {ok,[TS1]}=rrdtool:update(File,Counters,[{TS1,Data}]).

%%====================================================================
%% Internal functions
%%====================================================================

