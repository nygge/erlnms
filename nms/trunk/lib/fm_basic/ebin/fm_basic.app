{application, fm_basic,
 [{description,"Fault Management"},
  {vsn,"0.1"},
  {modules,[alarm_pp, alarm_pres, alarm_sts, alarm_sts_server, fm_basic, fm_mkTab, ne_al_sts, pp_action, pp_alarm, pp_hold, raw_alarm, subscription_event_h, sup_alarm_apps, sup_alarm, sup_alarm_pp, sup_alarms, sup_pp_proc, trap2alarm2, trap2alarm]},
  {applications,[kernel,stdlib]},
  {mod, {fm_basic,[]}},
  {env,[]}
 ]}.
