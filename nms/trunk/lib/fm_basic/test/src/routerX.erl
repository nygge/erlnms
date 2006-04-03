%%%-------------------------------------------------------------------
%%% File    : routerX.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 23 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(routerX).
-include("trap.hrl").
-export([trap1/1,trap2/1]).

trap1(Trap) ->
    [{ne,'host.domain.com'},{port,27}].

trap2(Trap) ->
    case Trap#trap.agentAddress of
	{127,10,0,16} ->
	    [{ne,'host.domain.com'},{port,hd(Trap#trap.varBindValues)}];
	{127,10,0,17} ->
	    [{ne,'mail.domain.com'},{port,27}]
    end.

