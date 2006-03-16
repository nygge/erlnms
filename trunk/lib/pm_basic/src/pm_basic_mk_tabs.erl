%%%-------------------------------------------------------------------
%%% File    : pm_basic_mk_tabs.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 14 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_basic_mk_tabs).

-export([all/0,all/1,
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

all() ->
    all([node()]).

all(Nodes) ->
    pm_store_inst(Nodes),
    pm_store_type(Nodes),
    pm_archive(Nodes),
    pm_aggregate(Nodes),
    pm_mo_type(Nodes),
    pm_counter(Nodes),
    pm_der_counter(Nodes),
    pm_duration(Nodes),
    pm_event(Nodes).


pm_store_inst(Nodes) ->
    mk_tab(Nodes,pm_store_inst,record_info(fields,pm_store_inst)).
pm_store_type(Nodes) ->
    mk_tab(Nodes,pm_store_type,record_info(fields,pm_store_type)).
pm_archive(Nodes) ->
    mk_tab(Nodes,pm_archive,record_info(fields,pm_archive)).
pm_aggregate(Nodes) ->
    mk_tab(Nodes,pm_aggregate,record_info(fields,pm_aggregate)).
pm_mo_type(Nodes) ->
    mk_tab(Nodes,pm_mo_type,record_info(fields,pm_mo_type)).
pm_counter(Nodes) ->
    mk_tab(Nodes,pm_counter,record_info(fields,pm_counter)).
pm_der_counter(Nodes) ->
    mk_tab(Nodes,pm_der_counter,record_info(fields,pm_der_counter));
pm_duration(Nodes) ->
    mk_tab(Nodes,pm_duration,record_info(fields,pm_duration)).
pm_event(Nodes) ->
    mk_tab(Nodes,pm_event,record_info(fields,pm_event)).
    
mk_tab(Nodes,Tab,Fields) ->
    mnesia:create_table(Tab,
			[{attributes, Fields},
			 {disc_copies,Nodes},{type,set}]).
    
