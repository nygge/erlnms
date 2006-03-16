%%%-------------------------------------------------------------------
%%% File    : pm_basic_mk_tabs.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 14 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_rrdtool_mkTabs).

-export([all/0,all/1,
	 pm_rrd_inst/1
	]).

-include("pm_rrdtool.hrl").

all() ->
    all([node()]).

all(Nodes) ->
    pm_rrd_inst(Nodes).

pm_rrd_inst(Nodes) ->
    mk_tab(Nodes,pm_rrd_inst,record_info(fields,pm_rrd_inst)).
    
mk_tab(Nodes,Tab,Fields) ->
    mnesia:create_table(Tab,
			[{attributes, Fields},
			 {disc_copies,Nodes},{type,set}]).
    
