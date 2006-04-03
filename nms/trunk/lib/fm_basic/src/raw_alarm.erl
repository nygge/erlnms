-module(raw_alarm).

-export([start_link/1,stop/1,send/1]).

%-----------------------------------------------------------------------------
%
%	EvtMgrName = Name | {Name,Node} | {global,Name} | pid()
%	EvtMgrName, the name to be given to the raw_alarm event manager

start_link(Evt_Mgr_Name) ->
   gen_event:start_link(Evt_Mgr_Name).

stop(Name) ->
   gen_event:stop(Name).

send(Event) ->
   gen_event:notify(?MODULE,Event).
