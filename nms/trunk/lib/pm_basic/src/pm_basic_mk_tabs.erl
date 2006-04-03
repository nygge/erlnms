%%%-------------------------------------------------------------------
%%% File    : pm_basic_mk_tabs.erl
%%% Created : 14 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Create mnesia tables for PM database backend based on RRDtool.
%%% @end
%%%-------------------------------------------------------------------
-module(pm_basic_mk_tabs).

-export([all/0,all/1,
	 pm_db_backend/1,
	 pm_store_inst/1,
         pm_store_type/1,
         pm_archive/1,
         pm_aggregate/1,
         pm_mo_type/1,
         pm_counter/1,
         pm_der_counter/1,
	 pm_duration/1,
	 pm_event/1
	]).

-include("pm_store.hrl").

%% @spec all() -> Res
%% @doc Create all tables, only on the current node.
%% @end
%% @equiv all([node()])
all() ->
    all([node()]).

%% @spec all(Nodes) -> Res
%% @doc Create all tables, on all nodes in Nodes.
all(Nodes) ->
    pm_db_backend(Nodes),
    pm_store_inst(Nodes),
    pm_store_type(Nodes),
    pm_archive(Nodes),
    pm_aggregate(Nodes),
    pm_mo_type(Nodes),
    pm_counter(Nodes),
    pm_der_counter(Nodes),
    pm_duration(Nodes),
    pm_event(Nodes).

%% @spec pm_db_backend(Nodes) -> Res
%% @doc Create table pm_db_backend
%% @end
pm_db_backend(Nodes) ->
    mk_tab(Nodes,pm_db_backend,record_info(fields,pm_db_backend)).

%% @spec pm_store_inst(Nodes) -> Res
%% @doc Create table pm_store_inst.
%% @end
pm_store_inst(Nodes) ->
    mk_tab(Nodes,pm_store_inst,record_info(fields,pm_store_inst)).

%% @spec pm_store_type(Nodes) -> Res
%% @doc Create table pm_store_type.
%% @end
pm_store_type(Nodes) ->
    mk_tab(Nodes,pm_store_type,record_info(fields,pm_store_type)).

%% @spec pm_archive(Nodes) -> Res
%% @doc Create table pm_archive.
%% @end
pm_archive(Nodes) ->
    mk_tab(Nodes,pm_archive,record_info(fields,pm_archive)).

%% @spec pm_aggregate(Nodes) -> Res
%% @doc Create table pm_aggregate.
%% @end
pm_aggregate(Nodes) ->
    mk_tab(Nodes,pm_aggregate,record_info(fields,pm_aggregate)).

%% @spec pm_mo_type(Nodes) -> Res
%% @doc Create table pm_mo_type
%% @end
pm_mo_type(Nodes) ->
    mk_tab(Nodes,pm_mo_type,record_info(fields,pm_mo_type)).

%% @spec pm_counter(Nodes) -> Res
%% @doc Create table pm_counter.
%% @end
pm_counter(Nodes) ->
    mk_tab(Nodes,pm_counter,record_info(fields,pm_counter)).

%% @spec pm_der_counter(Nodes) -> Res
%% @doc Create table pm_der_counter.
%% @end
pm_der_counter(Nodes) ->
    mk_tab(Nodes,pm_der_counter,record_info(fields,pm_der_counter)).

%% @spec pm_duration(Nodes) -> res
%% @doc Create table pm_duration.
%% @end
pm_duration(Nodes) ->
    mk_tab(Nodes,pm_duration,record_info(fields,pm_duration)).

%% @spec pm_event(Nodes) -> Res
%% @doc Create table pm_event.
%% @end
pm_event(Nodes) ->
    mk_tab(Nodes,pm_event,record_info(fields,pm_event)).
    
mk_tab(Nodes,Tab,Fields) ->
    mnesia:create_table(Tab,
			[{attributes, Fields},
			 {disc_copies,Nodes},{type,set}]).
    
