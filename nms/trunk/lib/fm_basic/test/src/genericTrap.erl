%%%-------------------------------------------------------------------
%%% File    : genericTrap.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 25 Jan 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(genericTrap).

-include("trap.hrl").
-export([linkdown/1,linkup/1]).

linkdown(Trap) ->
    linkupdown(Trap).

linkup(Trap) ->
    linkupdown(Trap).

linkupdown(Trap) ->
    [{ne,addr2name(Trap#trap.agentAddress)},{ifind,hd(Trap#trap.varBindValues)}].

addr2name(IP) ->
    'host.domain.com'.
