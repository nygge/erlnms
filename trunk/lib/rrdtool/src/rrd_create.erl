%%%-------------------------------------------------------------------
%%% File    : rrd_create.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2003-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%% @private
%%%
%%% Created : 26 Aug 2003 by Anders Nygren 
%%%-------------------------------------------------------------------
-module(rrd_create).

-export([do_create/2]).

-include("rrdtool.hrl").

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

do_create(Port,File) when is_record(File,rrd_file) ->
    CMD=create_cmd(File),
    rrd_lib:do_cmd(Port,CMD).

create_cmd(#rrd_file{file=File,start=Start,
		     step=Step,dss=DSs,rras=RRAs}) ->
    [<<"create ">>,
     list_to_binary(File++" "),
     start_to_binary(Start),
     step_to_binary(Step),
     dss_to_binary(DSs),
     rras_to_binary(Step,RRAs),
     <<$\n>>].

start_to_binary(undefined) ->
    [];
start_to_binary(DateTime) ->
    [<<"-b ">>,
     rrd_lib_utils:val_to_binary(time:datetime_to_epoch(DateTime)),
     <<" ">>].
step_to_binary(undefined) ->
    [];
step_to_binary(Duration) ->
    [<<"-s ">>,
     time:duration_to_binary(Duration),
     <<" ">>].

dss_to_binary(DSs) ->
    lists:map(fun(DS) ->
		      ds_to_binary(DS)
	      end, DSs).

ds_to_binary(#rrd_ds{name=Name,type=DST,hb=HB,min=Min,max=Max}) ->
    [rrd_lib_utils:vals_to_binary(["DS",Name,DST,
				   time:duration_to_seconds(HB),
				   Min,Max
				   ],":"),
     <<" ">>];

ds_to_binary(#rrd_ds_comp{name=Name,rpn=RPN}) ->
    [rrd_lib_utils:vals_to_binary(["DS",Name,"COMPUTE",RPN],":"),
     <<" ">>].

rras_to_binary(SampleInt,RRAs) ->
    lists:map(fun(RRA) ->
		      rra_to_binary(SampleInt,RRA)
	      end, RRAs).

rra_to_binary(SampleInt,#rrd_rra{cf=CF,xff=XFF,interval=ConsInt,
				 duration=Capacity}) 
  when is_tuple(Capacity) ->
    Step=time:duration_div(ConsInt,SampleInt),
    Rows=time:duration_div(Capacity,ConsInt),
    [rrd_lib_utils:vals_to_binary(["RRA",CF,
				   XFF,
				   Step,
				   Rows
				  ],":"),
     <<" ">>];

rra_to_binary(_SampleInt,#rrd_rra_hwpred{rows=Rows,alpha=Alpha,
					 beta=Beta,period=Period,
					 rra_num=Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["HWPREDICT",Rows,Alpha,Beta,
				   Period,Rra_num],":"),
     <<" ">>];
rra_to_binary(_SampleInt,#rrd_rra_seasonal{period=Period,gamma=Gamma,
					   rra_num=Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["SEASONAL",Period,Gamma,Rra_num],":"),
     <<" ">>];
rra_to_binary(_SampleInt,#rrd_rra_devseason{period=Period,gamma=Gamma,
					    rra_num=Rra_num}) when is_integer(Rra_num)->
    [rrd_lib_utils:vals_to_binary(["DEVSEASONAL",Period,Gamma,Rra_num
				  ],":"),
     <<" ">>];
rra_to_binary(_SampleInt,#rrd_rra_devpredict{rows=Rows,rra_num=Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["DEVPREDICT",Rows,Rra_num
				  ],":"),
     <<" ">>];
rra_to_binary(_SampleInt,#rrd_rra_failures{rows=Rows,
					   threshold=Threshold,
					   window=Window_len,
					   rra_num=Rra_num}) ->
    [rrd_lib_utils:vals_to_binary(["FAILURES",Rows,Threshold,Window_len,Rra_num
				  ],":"),
     <<" ">>].

get_sample_int([{step,Int}|_Opts]) ->
    Int;
get_sample_int([_Opt|More]) ->
    get_sample_int(More);
get_sample_int([]) ->
    ?DEF_SAMPLE_INT.
