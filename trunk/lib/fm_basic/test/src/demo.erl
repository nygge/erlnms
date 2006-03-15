-module(demo).

-include("CIM_SNMPTrapIndication.hrl").
-include("CIM_AlertIndication.hrl").
-include("trap.hrl").
%%-include("pm_rec.hrl").

-export([xx/0,ld/0,linkdown/0,linkup/0,
	 h1on/0,h1off/0,h2on/0,h2off/0,
	 trap2on/1,trap2off/1,
	 cpu/1,cpu_l/0,cpu_h/0,
	 mem/1,mem_l/0,mem_h/0,disc/1]).

linkdown() ->
    trap2alarm2:trap(#trap{indicationTime=calendar:local_time(),
			   enterprise=routerX,
			   agentAddress={127,10,0,16},
			   genericTrap=?LINKDOWN,
			   specificTrap=0,
			   varBindNames=[ifIndex,ifAdminStatus,ifOperStatus],
			   varBindSyntaxes=[?INTEGER,?INTEGER,?INTEGER],
			   varBindValues=[2,?UP,?DOWN]}).

linkup() ->
    trap2alarm2:trap(#trap{indicationTime=calendar:local_time(),
			   enterprise=routerX,
			   agentAddress={127,10,0,16},
			   genericTrap=?LINKUP,
			   specificTrap=0,
			   varBindNames=[ifIndex,ifAdminStatus,ifOperStatus],
			   varBindSyntaxes=[?INTEGER,?INTEGER,?INTEGER],
			   varBindValues=[2,?UP,?UP]}).

h1on()->
    trap2on({127,10,0,16}).

h1off()->
    trap2off({127,10,0,16}).

h2on()->
    trap2on({127,10,0,17}).

h2off()->
    trap2off({127,10,0,17}).


trap2on(Addr) ->
    trap2alarm2:trap(#trap{indicationTime=calendar:local_time(),
			   enterprise=routerX,
			   agentAddress=Addr,
			   genericTrap=?ENTERPRISESPECIFIC,
			   specificTrap=2,
			   varBindNames=[foo,bar,hepp],
			   varBindSyntaxes=[?INTEGER,?INTEGER,?INTEGER],
			   varBindValues=[1,3,5]}).

trap2off(Addr) ->
    trap2alarm2:trap(#trap{indicationTime=calendar:local_time(),
			   enterprise=routerX,
			   agentAddress=Addr,
			   genericTrap=?ENTERPRISESPECIFIC,
			   specificTrap=2,
			   varBindNames=[foo,bar,hepp],
			   varBindSyntaxes=[?INTEGER,?INTEGER,?INTEGER],
			   varBindValues=[1,4,5]}).

cpu_h()->
    cpu(95).

cpu_l()->
    cpu(80).

cpu(L)->
    pm_send([{ne,'host.domain.com'},{cpu,1}],cpu,[L]).

mem_h()->
    mem(95).

mem_l()->
    mem(80).

mem(L)->
    pm_send([{ne,'host.domain.com'},{mem,main}],mem,[L]).

disc(L) ->
    pm_send([{ne,'host.domain.com'},{partition,'/dev/hda1'}],disc,[L]).
    

pm_send(ME,CNT,VAL) ->
%%     perf_data:send(#pm_rec{me=ME,
%% 			   obj=CNT,
%% 			   ts=calendar:universal_time(),
%% 			   data=VAL}).
    ok.
ld()->
    A=#'CIM_AlertIndication'{'Description'="Link Blocked",
			     'AlertingManagedElement'="{ne1,link1}",
			     'AlertType'=?COMMUNICATIONALERT,
			     'OtherAlertType'=undefined, 
			     'PerceivedSeverity'=?MAJOR, 
			     'OtherSeverity'=undefined, 
			     'ProbableCause'=7, 
			     'ProbableCauseDescription'="Link Down",
			     'Trending'=?TRENDINGUP,
			     'RecommendedActions'="Deblock link",
			     'EventId'=176,
			     'EventTime'=calendar:local_time(),
			     'SystemCreationClassName'=undefined,
			     'SystemName'=undefined,
			     'ProviderName'=undefined},
    send(A).

xx()->
    A=#'CIM_AlertIndication'{'Description'="Something happened",
			     'AlertingManagedElement'="{ne1,link1}",
			     'AlertType'=?COMMUNICATIONALERT,
			     'OtherAlertType'=undefined, 
			     'PerceivedSeverity'=?MAJOR, 
			     'OtherSeverity'=undefined, 
			     'ProbableCause'=7, 
			     'ProbableCauseDescription'="Link Down",
			     'Trending'=?TRENDINGUP,
			     'RecommendedActions'="Deblock link",
			     'EventId'=176,
			     'EventTime'=calendar:local_time(),
			     'SystemCreationClassName'="AXD 301",
			     'SystemName'="Tlaloc",
			     'ProviderName'="Ericsson"},
    send(A).

send(A) ->
    raw_alarm:send(A).

