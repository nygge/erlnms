%%%-------------------------------------------------------------------
%%% File    : sup_pp_proc.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 10 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(sup_pp_proc).
-behaviour(supervisor).
-export([init/1]).

init(_) ->
   {ok,{{one_for_one, 5, 30},
        [
	 {pp_hold, {pp_hold, start_link, []}, transient, 5000, worker, [pp_hold]}
	,{pp_action, {pp_action, start_link, []}, transient, 5000, worker, [pp_action]}
	]}}.
