%%%-------------------------------------------------------------------
%%% File    : alarm_pp.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 10 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(alarm_pp).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-include("alarm.hrl").
-include("CIM_SNMPTrapIndication.hrl").
-include("CIM_AlertIndication.hrl").


-export([start/1,start_link/1,stop/1,subscribe/1,unsubscribe/0,send/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state,{val=[],alarm_cnt=0}).

%%=======================================================================
%%	Interface
%%
start(Name)-> 
   gen_server:start(Name, ?MODULE, [], []).
start_link(Name)-> 
   gen_server:start_link(Name, ?MODULE, [], []).
stop(Name)-> 
   gen_server:call(Name, stop, 10000).
subscribe(Filter) ->
   subscription_event_h:subscribe(alarm_pp_em,self(),Filter).

unsubscribe() ->
   subscription_event_h:unsubscribe(alarm_pp_em,self()).

% get_alarm_count() ->
%        gen_event:call(alarm_pp_em,get_alarm_count).

send(Alarm) ->
   gen_event:notify(alarm_pp_em,Alarm).


%%=======================================================================
%%      Callbacks
%%
init(Name)->
   %process_flag(trap_exit, true),
   Res=gen_event:start_link({local,alarm_pp_em}),
   Filter=mkFilter(alarm_pp_em),
   subscription_event_h:subscribe(raw_alarm,?MODULE,Filter),
   {ok, #state{}}.

handle_call(stop,From,State)->
   gen_event:stop(?MODULE),
   {stop,normal,ok,State};

handle_call(Msg,From,State)-> {reply,ok,State}.

handle_cast(Msg,State)-> {noreply,State}.

handle_info(Info,State)-> {noreply,State}.

terminate(Reason,State)-> ok.

code_change(OldVsn,State,Extra)-> {ok,State}.

%%=======================================================================
%%      Implementation
%%

mkFilter(Evt_Mgr_Name) ->
   ToMe=fun (X) -> gen_event:notify(Evt_Mgr_Name,X) end,
   ToHold={send_to,pp_hold},
   ToAction={send_to,pp_action},
   Filter=ets:fun2ms(fun (A) when is_record(A,'CIM_SNMPTrapIndication') and 
                                  ((A#'CIM_SNMPTrapIndication'.genericTrap==?LINKDOWN) or
                                   (A#'CIM_SNMPTrapIndication'.genericTrap==?LINKUP)) ->
                            {ToHold,{hold,A,5000}};

                         (A) when is_record(A,'CIM_AlertIndication'), 
			          (A#'CIM_AlertIndication'.'Description'
				   =="Link Blocked") ->
                            {ToAction,{act,unblock_link,start_link,A}};

 			 (A) when is_record(A,'CIM_ThresholdIndication') and
 				  (A#'CIM_ThresholdIndication'.'ThresholdIdentifier'
 				   ==disc) ->
 			     {ToAction,{act,ext_part,start_link,A}};
			     
% 			 (A) when is_record(A,'CIM_ThresholdIndication') andalso
% 				  (A#'CIM_ThresholdIndication'.'ThresholdIdentifier'
% 				   ==disc) andalso
% 				  (A#'CIM_ThresholdIndication'.'ObservedValue'>95) ->
% 			     {ToAction,{act,ext_part,start_link,A}};
			     
% 			 ({pm_alarm,Host,disc_space,Part,Val}) ->
% 			     {ToAction,{act,ext_part,start_link,{Host,Part}}};

                         (X) -> {ToMe,X} end).
