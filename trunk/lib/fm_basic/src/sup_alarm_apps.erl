-module(sup_alarm_apps).
-behaviour(supervisor).
-export([init/1]).

init(_) ->
   {ok,{{one_for_one, 5, 30},
        [
	 %{alarm_log, {alarm_log, start_link, [alarm_log]}, transient, 5000, worker, [alarm_log]},
         {sup_alarm_pp, {supervisor, start_link, [{local,sup_alarm_pp},sup_alarm_pp,[]]}, 
                  transient, 5000, supervisor, [sup_alarm_pp]}]}}.
