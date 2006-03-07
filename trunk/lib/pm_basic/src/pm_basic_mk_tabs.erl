%%%-------------------------------------------------------------------
%%% File    : pm_basic_mk_tabs.erl
%%% Author  : Anders <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 14 Jun 2004 by Anders <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_basic_mk_tabs).

-export([all/0,all/1,
	 pm_db_backend/1,
	 pm_duration/1,
	 pm_event/1
	]).

-include("pm_config.hrl").

all() ->
    all([node()]).

all(Nodes) ->
    pm_db_backend(Nodes),
    pm_duration(Nodes),
    pm_event(Nodes).

pm_db_backend(Nodes) ->
    mk_tab(Nodes,pm_db_backend,record_info(fields,pm_db_backend)).
pm_duration(Nodes) ->
    mk_tab(Nodes,pm_duration,record_info(fields,pm_duration)).
pm_event(Nodes) ->
    mk_tab(Nodes,pm_event,record_info(fields,pm_event)).
    
mk_tab(Nodes,Tab,Fields) ->
    mnesia:create_table(Tab,
			[{attributes, Fields},
			 {disc_copies,Nodes},{type,set}]).
    
