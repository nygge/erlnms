%%%-------------------------------------------------------------------
%%% File    : rrd_info.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_info).

-export([do_info/3]).

do_info(Port,File,Type) ->
    CMD=create_cmd(File),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,R} ->
	    Ret=parse_result(R),
	    case Type of
		all ->
		    Ret;
		last ->
		    element(4,Ret);
		dss ->
		    element(5,Ret);
		rra ->
		    element(6,Ret)
	    end;
	Error ->
	    Error
    end.

create_cmd(File) ->
    [<<"info ">>,
     list_to_binary(File++" "),
     <<$\n>>].

parse_result(Res) ->
    Lines=string:tokens(Res,"\n"),
    [[T1,FN],[T2,Vers],[T3,St],[T4,Last]|More]=
	lists:map(fun (X) ->
			  string:tokens(X," =\n\"")
		  end,
		  Lines),
    FileName={filename,FN},
    Version={version,Vers},
    StepSecs=list_to_integer(St),
    Step={step,rrd_lib_utils:seconds_to_duration(StepSecs)},
    LastUpdate={lastupdate,rrd_lib_utils:epoch_to_datetime(
			     list_to_integer(Last))},
    {DSs,Rest}=parse_dss(More),
    RSs=parse_rra(Rest,StepSecs),
    {FileName,Version,Step,LastUpdate,{dss,DSs},{rra,RSs}}.

parse_dss(Inp) ->
    parse_dss([],Inp).

parse_dss(Acc,[["ds"++X,V]|More]=Pars) ->
    parse_ds(Acc,Pars);
parse_dss(Acc,[["rra"++X,V]|More]=Pars) ->
    {lists:reverse(Acc),Pars}.

parse_ds(Acc,[[T1,Type1],[T2,HB1],[T3,Min1],[T4,Max1],
	      [T5,Last1],[T6,Val],[T7,Unknown]|DSs]) ->
    DS=get_ds(T1),
    Type={type,list_to_atom(Type1)},
    HB={heartbeat,rrd_lib_utils:seconds_to_duration(list_to_integer(HB1))},
    Min={min,minmax_to_val(Min1)},
    Max={max,minmax_to_val(Max1)},
    Last={last,last_to_val(Last1)},
    Value={value,val_to_val(Val)},
    Unknowns={unknown_sec,unknown_to_val(Unknown)},
    parse_dss([{DS,[Type,HB,Min,Max,Last,Value,Unknowns]}|Acc],DSs).

get_ds(Name) ->
    list_to_atom(lists:nth(2,string:tokens(Name,"[]"))).

minmax_to_val("UNKN") ->
    undefined;
minmax_to_val(Val) ->
    list_to_float(Val).

last_to_val("UNKN") ->
    unknown;
last_to_val(Val) ->
    list_to_float(Val).

val_to_val("NaN") ->
    unknown;
val_to_val(Val) ->
    list_to_float(Val).
unknown_to_val(Val) ->
    list_to_integer(Val).

parse_rra([[T1,CF1],[T2,Rows1],[T3,Pdppr],[T4,XFF1]|More],Step) ->
    RRA=hd(string:tokens(T1,".")),
    CF={cf,list_to_atom(CF1)},
    Rows=list_to_integer(Rows1),
    PperSampl=list_to_integer(Pdppr),
    DUR={duration,rrd_lib_utils:seconds_to_duration(Rows*Step*PperSampl)},
    RES={res,rrd_lib_utils:seconds_to_duration(Step*PperSampl)},
    XFF={xff,list_to_float(XFF1)},
    Rest=eat_cdp_prep(RRA,More),
    [{CF,DUR,RES,XFF}|parse_rra(Rest,Step)];
parse_rra([],_) ->
    [].
    
eat_cdp_prep(RRA,[[T1,Y]|More]=Input) ->
    case hd(string:tokens(T1,".")) of
	RRA ->
	    eat_cdp_prep(RRA,More);
	Other ->
	    Input
    end;
eat_cdp_prep(RRA,[]) ->
    [].
