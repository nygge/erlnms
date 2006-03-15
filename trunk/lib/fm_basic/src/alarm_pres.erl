%%%-------------------------------------------------------------------
%%% File    : alarm_pres.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 10 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(alarm_pres).

-export([start/0,start/1,stop/0,init/0,print/2,format/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("alarm.hrl").
-include("CIM_AlertIndication.hrl").
-include("CIM_SNMPTrapIndication.hrl").

start()->
    spawn(?MODULE,init,[]).

start(Lang)->
    spawn(?MODULE,init,[Lang]).

stop() ->
    ?MODULE!stop. 

print(Lang,Alarm) ->
    ?MODULE!{print_alarm,Lang,Alarm}.

format(Lang,Alarm) ->
    ?MODULE!{format_alarm,self(),Lang,Alarm},
    receive
	{formatted,L} ->
	    L
    end.

%init(Lang)->
init()->
    process_flag(trap_exit, true),
    Pid=self(),
    register(?MODULE,Pid),
    %connect(),
    loop().

loop()->
    receive
	stop ->
	    stop;
	reconnect ->
	    connect();
	{print_alarm,Lang,Alarm} ->
	    print_alarm(Alarm,Lang);
	{format_alarm,Pid,Lang,Alarm} ->
	    Pid!format_alarm(Alarm,Lang);
	{'EXIT',Pid,Reason} ->
	    timer:send_after(2000,self(),reconnect);
	X ->
	    print_alarm(X,en)
    end,
    loop().

connect()->
    Pid=self(),
    Filter=ets:fun2ms(fun (X)->{{send_to,Pid},X} end),
    alarm_pp:subscribe(Filter).

format_alarm(Alarm,Lang) when is_record(Alarm,'CIM_AlertIndication') ->
    AList=tuple_to_list(Alarm),
    format('CIM_AlertIndication',AList,Lang).

format(Type,List,Lang) ->
    format(Type,List,Lang,1,[]).

format(Type,[A|As],Lang,1,L) ->
    format(Type,As,Lang,2,L);
format(Type,[undefined|As],Lang,N,L) ->
    format(Type,As,Lang,N+1,L);
format(Type,[A|As],Lang,N,L) ->
    format(Type,As,Lang,N+1,[format(Type,N,Lang,A)|L]);
format(Type,[],Lang,N,L) ->
    L.

format(Type,Key,Lang,Val) ->
    case nls:getString(Type,Key,Lang) of
	{found,String} ->
	    io_lib:format("~s: ~s~n",[String,Val]);
	_ ->
	    io_lib:format("UNDEFINED ~w/~w/~w~n",[Type,Key,Lang])
    end.

    
print_alarm (X,Lang) when is_record(X,'CIM_AlertIndication') ->
    print_hd(),
    printIndicationIdentifier(Lang,X#'CIM_AlertIndication'.'IndicationIdentifier'),
    printCorrelatedIndications(Lang,X#'CIM_AlertIndication'.'CorrelatedIndications'),
    printIndicationTime(Lang,X#'CIM_AlertIndication'.'IndicationTime'),
    printDescription(Lang,X#'CIM_AlertIndication'.'Description'),
    printAlertingManagedElement(Lang,X#'CIM_AlertIndication'.'AlertingManagedElement'),
    printAlertType(Lang,X#'CIM_AlertIndication'.'AlertType'), 
    printOtherAlertType(Lang,X#'CIM_AlertIndication'.'OtherAlertType'), 
    printPerceivedSeverity(Lang,X#'CIM_AlertIndication'.'PerceivedSeverity'), 
    printOtherSeverity(Lang,X#'CIM_AlertIndication'.'OtherSeverity'), 
    printProbableCause(Lang,X#'CIM_AlertIndication'.'ProbableCause'), 
    printProbableCauseDescription(Lang,X#'CIM_AlertIndication'.'ProbableCauseDescription'),
    printTrending(Lang,X#'CIM_AlertIndication'.'Trending'), 
    printRecommendedActions(Lang,X#'CIM_AlertIndication'.'RecommendedActions'), 
    printEventId(Lang,X#'CIM_AlertIndication'.'EventId'), 
    printEventTime(Lang,X#'CIM_AlertIndication'.'EventTime'), 
    printSystemCreationClassName(Lang,X#'CIM_AlertIndication'.'SystemCreationClassName'), 
    printSystemName(Lang,X#'CIM_AlertIndication'.'SystemName'), 
    printProviderName(Lang,X#'CIM_AlertIndication'.'ProviderName');

print_alarm (X,Lang) when is_record(X,'CIM_SNMPTrapIndication') ->
    print_hd(),
    printIndicationIdentifier(Lang,X#'CIM_SNMPTrapIndication'.'IndicationIdentifier'),
    printCorrelatedIndications(Lang,X#'CIM_SNMPTrapIndication'.'CorrelatedIndications'),
    printIndicationTime(Lang,X#'CIM_SNMPTrapIndication'.'IndicationTime'),
    print_enterprise(Lang,X#'CIM_SNMPTrapIndication'.enterprise),
    print_agentAddress(Lang,X#'CIM_SNMPTrapIndication'.agentAddress),
    print_genericTrap(Lang,X#'CIM_SNMPTrapIndication'.genericTrap),
    print_specificTrap(Lang,X#'CIM_SNMPTrapIndication'.specificTrap),
    print_timeStamp(Lang,X#'CIM_SNMPTrapIndication'.timeStamp),
    print_varBindNames(Lang,X#'CIM_SNMPTrapIndication'.varBindNames),
    print_varBindSyntaxes(Lang,X#'CIM_SNMPTrapIndication'.varBindSyntaxes),
    print_varBindValues(Lang,X#'CIM_SNMPTrapIndication'.varBindValues);

print_alarm (X,Lang) when is_record(X,'CIM_ThresholdIndication') ->
    print_alarm(X#'CIM_ThresholdIndication'.'CIM_AI',Lang),
    print_thresholdIdentifier(Lang,X#'CIM_ThresholdIndication'.'ThresholdIdentifier'),
    print_thresholdValue(Lang,X#'CIM_ThresholdIndication'.'ThresholdValue'),    
    print_observerValue(Lang,X#'CIM_ThresholdIndication'.'ObservedValue');

print_alarm(X,Lang) ->
    io:format("Alarm ~w~n",[X]).

print_hd() ->
    io:format("===== New Alarm =======================================~n",[]).
printIndicationIdentifier(Lang,undefined) ->
    ok;
printIndicationIdentifier(Lang,X) ->
    print('CIM_AlertIndication',?INDICATIONIDENTIFIER,Lang,X).

printCorrelatedIndications(Lang,undefined) ->
    ok;
printCorrelatedIndications(Lang,X) ->
    print('CIM_AlertIndication',?CORRELATEDINDICATIONS,Lang,X).

printIndicationTime (Lang,{{YY,MO,DD},{HH,MI,SS}}) -> 
    {found,Str}=nls:getString('CIM_AlertIndication',?INDICATIONTIME,Lang),
    io:format("~s: ~w/~w/~w, ~w:~w:~w~n",[Str,YY,MO,DD,HH,MI,SS]);
printIndicationTime (Lang,undefined) ->
    ok.

printDescription (Lang,undefined) ->
    ok;
printDescription (Lang,X) ->
    print('CIM_AlertIndication',?DESCRIPTION,Lang,X).

printAlertingManagedElement (Lang,undefined) ->
    ok;
printAlertingManagedElement (Lang,X) ->
    print('CIM_AlertIndication',?ALERTINGMANAGEDELEMENT,Lang,X).

printAlertType (Lang,undefined) -> 
    ok;
printAlertType (Lang,X) -> 
    {found,String}=nls:getString(alert_type,X,Lang),
    print('CIM_AlertIndication',?ALERTTYPE,Lang,String).

printOtherAlertType (Lang,undefined) -> 
    ok;
printOtherAlertType (Lang,X) -> 
    print('CIM_AlertIndication',?OTHERALERTTYPE,Lang,X).

printPerceivedSeverity (Lang,undefined) -> 
    ok;
printPerceivedSeverity (Lang,X) -> 
    {found,String}=nls:getString(perceived_severity,X,Lang),
    print('CIM_AlertIndication',?PERCEIVEDSEVERITY,Lang,String).

printOtherSeverity (Lang,undefined) -> 
    ok;
printOtherSeverity (Lang,X) -> 
    print('CIM_AlertIndication',?OTHERSEVERITY,Lang,X).

printProbableCause (Lang,undefined) -> 
    ok;
printProbableCause (Lang,X) -> 
    {found,String}=nls:getString(probable_cause,X,Lang),
    print('CIM_AlertIndication',?PROBABLECAUSE,Lang,String).

printProbableCauseDescription (Lang,undefined) -> 
    ok;
printProbableCauseDescription (Lang,X) -> 
    print('CIM_AlertIndication',?PROBABLECAUSEDESCRIPTION,Lang,X).
printTrending (Lang,undefined) -> 
    ok;
printTrending (Lang,X) -> 
    {found,String}=nls:getString(trending,X,Lang),
    print('CIM_AlertIndication',?TRENDING,Lang,String).
printRecommendedActions (Lang,undefined) -> 
    ok;
printRecommendedActions (Lang,X) -> 
    print('CIM_AlertIndication',?RECOMMENDEDACTIONS,Lang,X).
printEventId (Lang,undefined) -> 
    ok;
printEventId (Lang,X) when is_integer(X) -> 
    print('CIM_AlertIndication',?EVENTID,Lang,integer_to_list(X));
printEventId (Lang,X) -> 
    print('CIM_AlertIndication',?EVENTID,Lang,X).
printEventTime (Lang,undefined) -> 
    ok;
printEventTime (Lang,{{YY,MO,DD},{HH,MI,SS}}) -> 
    {found,Str}=nls:getString('CIM_AlertIndication',?EVENTTIME,Lang),
    io:format("~s: ~w/~w/~w, ~w:~w:~w~n",[Str,YY,MO,DD,HH,MI,SS]).
printSystemCreationClassName (Lang,undefined) -> 
    ok;
printSystemCreationClassName (Lang,X) -> 
    print('CIM_AlertIndication',?SYSTEMCREATIONCLASSNAME,Lang,X).
printSystemName (Lang,undefined) -> 
    ok;
printSystemName (Lang,X) -> 
    print('CIM_AlertIndication',?SYSTEMNAME,Lang,X).
printProviderName  (Lang,undefined) ->
    ok;
printProviderName  (Lang,X) ->
    print('CIM_AlertIndication',?PROVIDERNAME,Lang,X).

print_thresholdIdentifier(Lang,X) ->    
    print('CIM_ThresholdIndication',?THRESHOLDIDENTIFIER,Lang,X).
print_thresholdValue(Lang,X) ->
    print('CIM_ThresholdIndication',?THRESHOLDVALUE,Lang,integer_to_list(X)).
print_observerValue(Lang,X) ->
    print('CIM_ThresholdIndication',?OBSERVEDVALUE,Lang,integer_to_list(X)).

print(Type,Key,Lang,Val) ->
    case nls:getString(Type,Key,Lang) of
	{found,String} ->
	    io:format("~s: ~s~n",[String,Val]);
	_ ->
	    io:format("UNDEFINED ~w/~w/~w~n",[Type,Key,Lang])
    end.
%%==================================================================
%%	CIM_SNMPTrap
%%
print_enterprise(Lang,X) ->
    io:format("enterprise ~w~n",[X]).
print_agentAddress(Lang,X) ->
    io:format("agentAddress ~w~n",[X]).
print_genericTrap(Lang,?COLDSTART) ->
    io:format("genericTrap ColdStart~n",[]);
print_genericTrap(Lang,?WARMSTART) ->
    io:format("genericTrap WarmStart~n",[]);
print_genericTrap(Lang,?LINKDOWN) ->
    io:format("genericTrap LinkDown~n",[]);
print_genericTrap(Lang,?LINKUP) ->
    io:format("genericTrap LinkUp~n",[]);
print_genericTrap(Lang,?AUTHENTICATIONFAILURE) ->
    io:format("genericTrap AuthenticationFailure~n",[]);
print_genericTrap(Lang,?EQPNEIGHBORLOSS) ->
    io:format("genericTrap EqpNeighborloss~n",[]);
print_genericTrap(Lang,?ENTERPRISESPECIFIC) ->
    io:format("genericTrap EnterpriseSpecific~n",[]);
print_genericTrap(Lang,X) ->
    io:format("genericTrap Illegal Trap value~n",[]).
print_specificTrap(Lang,X) ->
    io:format("specificTrap ~w~n",[X]).
print_timeStamp(Lang,{{YY,MO,DD},{HH,MI,SS}}) -> 
    io:format("timeStamp ~w/~w/~w, ~w:~w:~w~n",[YY,MO,DD,HH,MI,SS]).
print_varBindNames(Lang,X) ->
    io:format("varBindNames ~w~n",[X]).
print_varBindSyntaxes(Lang,X) ->
    io:format("varBindSyntaxes ~w~n",[X]).
print_varBindValues(Lang,X) ->
    io:format("varBindValues ~w~n",[X]).
