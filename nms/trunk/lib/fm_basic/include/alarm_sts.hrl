%%%-------------------------------------------------------------------
%%% File    : alarm_sts.hrl
%%% Author  : Anders Nygren <anders@local>
%%% Description : 
%%%
%%% Created : 11 Aug 2003 by Anders Nygren <anders@local>
%%%-------------------------------------------------------------------

-record(as_alarm,{id,ack=nack,alarm,comments=[]}).
-record(ack,{who,time}).

%------------------------------------------
% Messages sent by alarm_sts to subscribers

-record(as_new,{id,alarm}).
-record(as_cease,{id,time}).
-record(as_clear,{id,who,time}).
-record(as_ack,{id,who,time}).
-record(as_comment,{id,who,comment}).


%%%==================================================================
%%%
%%%    NOT USED
%%%

-record(severity,{fatal=0,
		  critical=0,
		  major=0,
		  minor=0,
		  degraded=0,
		  info=0,
		  other=0,
		  unknown=0}).

-record(alarm_sts, {ack=#severity{},
		    nack=#severity{}}).
