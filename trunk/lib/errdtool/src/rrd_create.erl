%%%-------------------------------------------------------------------
%%% File    : rrd_create.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 26 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_create).

-export([do_create/5]).

-define(DEF_SAMPLE_INT,{min,5}).

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
%%
%%------------------------------------------------
%% Data Source definition
%%
%% DSs=[{Name,DST,HB,Min,Max}]
%% Name        = Data Source Name, 
%%             = string | atom
%% DST         = Data Source Type
%%             = 'GAUGE' | 'COUNTER' | 'DERIVE' | 'ABSOLUTE'
%% HB          = Duration, Max number of seconds between two updates
%% Min         = integer, Min allowed value
%% Max         = integer, Max allowed value
%%------------------------------------------------
%% Round Robin Archive definition
%%
%% RRAs=[{'CF',XFF,ConsInt,ArchiveTime}].
%% CF          = Consolidation Function
%%             = 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'
%% XFF         = X-Files Factor, 0<XFF<1
%% ConsInt     = {Unit,No}, Consolidation Interval
%% ArchiveTime = {Unit,No}, Time duration that the archive contains
%% Unit        = sec | min | hour | day | week | month | year
%% No          = integer

do_create(Port,File,Opts,DSs,RRAs) ->
    CMD=create_cmd(File,Opts,DSs,RRAs),
    rrd_lib:do_cmd(Port,CMD).

create_cmd(File,Opts,DSs,RRAs) ->
     SampleInt=get_sample_int(Opts),
    [<<"create ">>,
     list_to_binary(File++" "),
     opts_to_binary(Opts),
     dss_to_binary(DSs),
     rras_to_binary(SampleInt,RRAs),
     <<$\n>>].

opts_to_binary(Opts) ->
    lists:map(fun(Opt)->
		      opt_to_binary(Opt)
	      end, Opts).

opt_to_binary({start,DateTime}) ->
    [<<"-b ">>,
     rrd_lib_utils:val_to_binary(rrd_lib_utils:datetime_to_epoch(DateTime)),
     <<" ">>];
opt_to_binary({step,Duration}) ->
    [<<"-s ">>,
     rrd_lib_utils:duration_to_binary(Duration),
     <<" ">>].

dss_to_binary(DSs) ->
    lists:map(fun(DS) ->
		      ds_to_binary(DS)
	      end, DSs).

ds_to_binary({Name,DST,HB,Min,Max}) ->
    [rrd_lib_utils:vals_to_binary(["DS",Name,DST,
				   rrd_lib_utils:duration_to_seconds(HB),
				   Min,Max
				   ],":"),
     <<" ">>];

ds_to_binary({Name,'COMPUTE',RPN}) ->
    [rrd_lib_utils:vals_to_binary(["DS",Name,"COMPUTE",RPN],":"),
     <<" ">>].

rras_to_binary(SampleInt,RRAs) ->
    lists:map(fun(RRA) ->
		      rra_to_binary(SampleInt,RRA)
	      end, RRAs).

rra_to_binary(SampleInt,{CF,XFF,ConsInt,Capacity}) when is_tuple(Capacity) ->
    Step=rrd_lib_utils:duration_div(ConsInt,SampleInt),
    Rows=rrd_lib_utils:duration_div(Capacity,ConsInt),
    [rrd_lib_utils:vals_to_binary(["RRA",CF,
				   XFF,
				   Step,
				   Rows
				  ],":"),
     <<" ">>];

rra_to_binary(_SampleInt,{'HWPREDICT',Rows,Alpha,Beta,Seasonal_period,Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["HWPREDICT",Rows,Alpha,Beta,
				   Seasonal_period,Rra_num],":"),
     <<" ">>];
rra_to_binary(_SampleInt,{'SEASONAL',Seasonal_period,Gamma,Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["SEASONAL",Seasonal_period,Gamma,Rra_num
				  ],":"),
     <<" ">>];
rra_to_binary(_SampleInt,{'DEVSEASONAL',Seasonal_period,Gamma,Rra_num}) when is_integer(Rra_num)->
    [rrd_lib_utils:vals_to_binary(["DEVSEASONAL",Seasonal_period,Gamma,Rra_num
				  ],":"),
     <<" ">>];
rra_to_binary(_SampleInt,{'DEVPREDICT',Rows,Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["DEVPREDICT",Rows,Rra_num
				  ],":"),
     <<" ">>];
rra_to_binary(_SampleInt,{'FAILURES',Rows,Threshold,Window_len,Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["FAILURES",Rows,Threshold,Window_len,Rra_num
				  ],":"),
     <<" ">>].

get_sample_int([{step,Int}|_Opts]) ->
    Int;
get_sample_int([_Opt|More]) ->
    get_sample_int(More);
get_sample_int([]) ->
    ?DEF_SAMPLE_INT.
