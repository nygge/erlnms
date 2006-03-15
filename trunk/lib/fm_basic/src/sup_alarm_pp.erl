%%%-------------------------------------------------------------------
%%% File    : sup_alarm_pp.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 10 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(sup_alarm_pp).
-behaviour(supervisor).
-export([init/1]).

init(_) ->
    {ok,{{one_for_one, 5, 30},
        [
	 {alarm_pp, {alarm_pp, start_link, [{local,alarm_pp}]},
	  transient, 5000, worker, [alarm_pp]},
	 {alarm_sts_server, {alarm_sts_server, start_link, 
			     [{local,alarm_sts_server}]},
	  transient, 5000, worker, [alarm_sts_server]},
	 {sup_pp_proc, {supervisor, start_link, [{local,sup_pp_proc},sup_pp_proc,[]]}, 
	  transient, 5000, supervisor, [sup_pp_proc]}
        ]}}.
