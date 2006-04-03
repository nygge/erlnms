%%%-------------------------------------------------------------------
%%% File    : pm_basic_mk_tabs.erl
%%% Created : 14 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Create mnesia tables for PM database backend based on RRDtool.
%%% @end
%%%-------------------------------------------------------------------
-module(pm_rrdtool_mkTabs).

-export([all/0,all/1,
	 pm_rrd_inst/1
	]).

-include("pm_rrdtool.hrl").

%% @spec all() -> Res
%% @doc Create all tables, only on the current node.
%% @end
%% @equiv all([node()])
all() ->
    all([node()]).

%% @spec all(Nodes) -> Res
%% @doc Create all tables, on all nodes in Nodes.
all(Nodes) ->
    pm_rrd_inst(Nodes).

%% @spec pm_rrd_inst(Nodes) -> Res
%% @doc Create table pm_rrd_inst.
pm_rrd_inst(Nodes) ->
    mk_tab(Nodes,pm_rrd_inst,record_info(fields,pm_rrd_inst)).
    
mk_tab(Nodes,Tab,Fields) ->
    mnesia:create_table(Tab,
			[{attributes, Fields},
			 {disc_copies,Nodes},{type,set}]).
    
