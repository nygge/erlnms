%%%-------------------------------------------------------------------
%%% File    : sup_alarms.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 10 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(sup_alarms).
-behaviour(supervisor).
-export([init/1]).

init(_) ->
   {ok,{{one_for_one, 5, 30},
        [{raw_alarm, {raw_alarm, start_link, [{local,raw_alarm}]},
	             transient, 5000, worker, [raw_alarm]}
        ,{trap2alarm, {trap2alarm, start_link, []},
	             transient, 5000, worker, [trap2alarm3]}
	,{sup_alarm_apps, {supervisor, start_link, 
	                   [{local,sup_alarm_apps},sup_alarm_apps,[]]}, 
                          transient, 5000, supervisor, [sup_alarm_apps]}
        ]}}.
