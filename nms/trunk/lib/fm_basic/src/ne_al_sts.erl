%%%-------------------------------------------------------------------
%%% File    : ne_al_sts.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 25 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(ne_al_sts).

-include("alarm_sts.hrl").
-include("CIM_AlertIndication.hrl").

-export([get_all/2]).
 
get_all(NE,Lang) ->
    {ok,Pid}=alarm_sts_server:get_pid(NE),
    Alarms=alarm_sts:get_sts(Pid),
    Rows=alarms_to_string(Alarms,Lang),
    SortedRows=lists:sort(fun (X,Y)->
				  order_alarms(X,Y)
			  end,Rows),
    mkTab(SortedRows).
%mkTab(["Ack","MO","Severity","Type","Probable Cause","Trend"],SortedRows).

order_alarms([A1,I1,S1,T1|More1]=X,[A2,I2,S2,T2|More2]=Y) when S1<S2->
    true;
order_alarms([A1,I1,S,T1|More1]=X,[A2,I2,S,T2|More2]=Y) when I1<I2->
    true;
order_alarms([A1,I,S,T1|More1]=X,[A2,I,S,T2|More2]=Y) when T1<T2->
    true;
order_alarms([A1,I1,S1,T1|More1]=X,[A2,I2,S2,T2|More2]=Y) ->
    false.

alarms_to_string(Alarms,Lang) ->
    lists:map(fun (Al) -> 
		      alarm_to_string(Al,Lang)
	      end, Alarms).

alarm_to_string(#as_alarm{id=Id,ack=Ack,alarm=A,comments=_},Lang) ->
    [alarm_list:ack_to_list(Ack),
     alarm_list:get_attr(A,'AlertingManagedElement',Lang),
     alarm_list:get_attr(A,'PerceivedSeverity',Lang),
     alarm_list:get_attr(A,'AlertType',Lang),
     alarm_list:get_attr(A,'ProbableCause',Lang),
     alarm_list:get_attr(A,'Trending',Lang)].
    
mkTab(Head,Alarms) ->
    {table, [{border,1}],
     {thead,[],mkRow(Head)},
     {tbody,[],mkRows(Alarms)}}.
    
mkTab(Alarms) ->
    {table, [{border,1}],
     {tbody,[],mkRows(Alarms)}}.

mkRows(Alarms) ->
    lists:map(fun (Al) ->
		      mkRow(Al)
	      end, Alarms).
mkRow(Alarm) ->
    {tr,[{bgcolor,get_color(lists:nth(3,Alarm))}],lists:map(fun (D) ->
			     {td,[],D}
		     end, Alarm)}.
get_color("Critical") ->
    red;
get_color("Major") ->
    yellow;
get_color("Minor") ->
    fuchsia;
get_color(X) ->
    white.
