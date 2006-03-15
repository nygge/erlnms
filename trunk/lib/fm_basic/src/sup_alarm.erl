-module(sup_alarm).
-behaviour(supervisor).
-export([init/1]).

init(_) ->
   {ok,{{one_for_all, 5, 30},
        [{raw_alarm, {event, start_link, [raw_alarm]}, transient, 5000, worker, [event]},
         {sup_alarm_apps, {supervisor, start_link, [{local,sup_alarm_apps},sup_alarm_apps,[]]}, 
                  transient, 5000, supervisor, [sup_alarm_apps]}]}}.
