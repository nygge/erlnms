%%%-------------------------------------------------------------------
%%% File    : threshold_worker.erl
%%% Created : 16 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Threshold worker process.
%%% @end
%%%-------------------------------------------------------------------
-module(threshold_worker).

-export([spawn/1,init/1]).

-include("threshold.hrl").
-include("threshold_crossing_evt.hrl").
-include("new_pm_data.hrl").

%% @spec spawn(Data) -> pid()
%% @doc Spawn the worker process.
spawn(Data) ->
    proc_lib:spawn(?MODULE,init,[Data]).

%% @private
init(#new_pm_data{moi=MOI,moc=MOC,int=Step,time=Time}) ->
    case threshold_conf:get_cmds(ws,MOI,MOC,Step) of
	Cmds when is_record(Cmds,th_cmd) ->
	    Cs=[C||{C,_}<-Cmds#th_cmd.cmds],
	    {Meta,Res}=pm_store:fetch(MOI,MOC,Cs,'AVERAGE',Step,2,Time),
	    {legends,CNames}=lists:nth(6,Meta),
	    [{PT,PVals},{CT,CVals}]=Res,
	    Trends=trends(PVals,CVals),
%	    [{Time,CVals}|X]=Res,
	    Vs=lists2:zip(CNames,CVals,Trends),
	    lists:foreach(fun ({C,V,Trend}) ->
				  check_threshold(MOI,MOC,C,V,Trend,Time,Step)
			  end,Vs);
	not_found ->
	    ignore
    end.

trends(Prev,Curr) ->
    lists:map(fun trend/1,lists2:zip(Prev,Curr)).

trend({Prev,Curr}) when Prev>Curr ->
    down;
trend({Prev,Curr}) when Prev<Curr ->
    up;
trend({Prev,Curr}) when Prev==Curr->
    same.
		      
check_threshold(MOI,MOC,C,V,Trend,Time,Step) ->
    case threshold_conf:check_threshold(ws,MOI,MOC,C,V,Step) of
	R when is_record(R,th_threshold) ->
	    send_th_crossing(MOI,MOC,C,V,Trend,Time,Step,R);
	not_found ->
	    ignore
    end.

send_th_crossing(MOI,MOC,C,V,Trend,Time,Step,R) ->
    Evt=#threshold_crossing_evt{moi=MOI,moc=MOC,cnt=C,value=V,int=Step,
				th_id=R#th_threshold.dests,
				llimit=R#th_threshold.min,
				ulimit=R#th_threshold.max},
    io:fwrite("~p~n",[Evt]).
