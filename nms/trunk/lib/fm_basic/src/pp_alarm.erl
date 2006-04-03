%%%-------------------------------------------------------------------
%%% File    : pp_alarm.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 10 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(alarm_pp).

-export([start_link/2,stop/1,subscribe/1,unsubscribe/0,send/1]).
-include_lib("stdlib/include/ms_transform.hrl").

%-----------------------------------------------------------------------------
%
%	EvtMgrName = Name | {Name,Node} | {global,Name} | pid()
%	EvtMgrName, the name to be given to the raw_alarm event manager

start_link({local,Evt_Mgr_Name}=Name,A_Name) ->
   gen_event:start_link(Name),
   pp_hold:start_link([]),
   Filter=mkFilter(Evt_Mgr_Name),
   subscription_event_h:subscribe(A_Name,?MODULE,Filter).

stop(Name) ->
   gen_event:stop(Name).

subscribe(Filter) ->
   subscription_event_h:subscribe(?MODULE,self(),Filter).

unsubscribe() ->
   subscription_event_h:unsubscribe(?MODULE,self()).

send(Alarm) ->
   gen_event:notify(?MODULE,Alarm).

mkFilter(Evt_Mgr_Name) ->
   ToMe=fun (X) -> gen_event:notify(Evt_Mgr_Name,X) end,
   %Hold=whereis(pp_hold),
   ToHold=fun (X) -> pp_hold!{hold,X,5000} end,
   Filter=ets:fun2ms(fun ({alarm,Class,Id,down,Severity}) ->
                            {ToHold,{alarm,Class,Id,down,Severity}};
                         (X) -> {ToMe,X} end).
