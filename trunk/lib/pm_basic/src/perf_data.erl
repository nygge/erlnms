%%% @private
-module(perf_data).

-export([start_link/1,stop/1,send/1,subscribe/1]).

%-----------------------------------------------------------------------------
%
%	EvtMgrName = Name | {Name,Node} | {global,Name} | pid()
%	EvtMgrName, the name to be given to the perf_data event manager

start_link(Evt_Mgr_Name) ->
   gen_event:start_link(Evt_Mgr_Name).

stop(Name) ->
   gen_event:stop(Name).

send(Event) ->
    case catch gen_event:notify(?MODULE,Event) of
	ok ->
	    true;
	{'EXIT',Stack} ->
	    false
    end.

subscribe(Filter) ->
   subscription_event_h:subscribe(perf_data,self(),Filter).
