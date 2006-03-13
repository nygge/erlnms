%%%-------------------------------------------------------------------
%%% File    : rrdtool.hrl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Feb 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------


%%------------------------------------------------
%%
%% create(Port,File,Opts,DSs,RRAs) -> {ok,nothing} | {error,Reason}
%%
%% FileName    = string, name of the file
%% Opts        = [Opt]
%% Opt         = {start,DateTime} | {step, Duration}
%% DateTime    = {{YYYY,MM,DD},{HH,MM,SS}}
%% Duration    = {Unit,Integer}
%% Unit        = sec | min | hour | day | week | month | year

-record(rrd_file,{file,start,step,dss,rras}).

%%------------------------------------------------
%% Data Source definition
%%
%% DS = {Name,DST,HB,Min,Max}
%% Name        = Data Source Name, 
%%             = string | atom
%% DST         = Data Source Type
%%             = 'GAUGE' | 'COUNTER' | 'DERIVE' | 'ABSOLUTE'
%% HB          = Duration, Max number of seconds between two updates
%% Min         = integer, Min allowed value
%% Max         = integer, Max allowed value

-record(rrd_ds,{name,type,hb,min,max}).

-record(rrd_ds_comp,{name,rpn}).

%%------------------------------------------------
%% Round Robin Archive definition
%%
%% RRA = {'CF',XFF,ConsInt,ArchiveTime}
%% CF          = Consolidation Function
%%             = 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'
%% XFF         = X-Files Factor, 0<XFF<1
%% ConsInt     = {Unit,No}, Consolidation Interval
%% ArchiveTime = {Unit,No}, Time duration that the archive contains
%% Unit        = sec | min | hour | day | week | month | year
%% No          = integer

-record(rrd_rra, {id,cf,xff,interval,duration}).

-record(rrd_rra_hwpred,{id,rows,alpha,beta,period,rra_num}).

-record(rrd_rra_seasonal,{id,period,gamma,rra_num}).

-record(rrd_rra_devseason,{id,period,gamma,rra_num}).

-record(rrd_rra_devpredict,{id,rows,rra_num}).

-record(rrd_rra_failures,{id,rows,threshold,window,rra_num}).
