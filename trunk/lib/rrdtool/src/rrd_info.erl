%%%-------------------------------------------------------------------
%%% File    : rrd_info.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2003-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%% @private
%%%
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_info).

-export([do_info/3]).

-include("rrdtool.hrl").

do_info(Port,File,Type) ->
    CMD=create_cmd(File),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,R} ->
	    Ret=parse_result(R),
	    case Type of
		all ->
		    Ret;
		last ->
		    Ret#rrd_file.last;
		dss ->
		    Ret#rrd_file.dss;
		rras ->
		    Ret#rrd_file.rras
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
    [[_T1,FileName],[_T2,_Vers],[_T3,St],[_T4,Last]|More]=
	lists:map(fun (X) ->
			  string:tokens(X," =\n\"")
		  end,
		  Lines),
    StepSecs=list_to_integer(St),
    Step=time:seconds_to_duration(StepSecs),
    LastUpdate=time:epoch_to_datetime(list_to_integer(Last)),
    {DSs,Rest}=parse_dss(More),
    RRAs=parse_rra(Rest,StepSecs),
    #rrd_file{file=FileName,
	      step=Step,
	      last=LastUpdate,
	      dss=DSs,
	      rras=RRAs}.

parse_dss(Inp) ->
    parse_dss([],Inp).

parse_dss(Acc,[["ds"++_X,_V]|_More]=Pars) ->
    parse_ds(Acc,Pars);
parse_dss(Acc,[["rra"++_X,_V]|_More]=Pars) ->
    {lists:reverse(Acc),Pars}.

parse_ds(Acc,[[T1,Type1],[_T2,HB1],[_T3,Min1],[_T4,Max1],
	      [_T5,Last1],[_T6,Val],[_T7,Unknown]|DSs]) ->
    DS=#rrd_ds{name=get_ds(T1),
	       type=list_to_atom(Type1),
	       hb=time:seconds_to_duration(list_to_integer(HB1)),
	       min=minmax_to_val(Min1),
	       max=minmax_to_val(Max1),
	       last=last_to_val(Last1),
	       value=val_to_val(Val),
	       unknown_sec=unknown_to_val(Unknown)},
    parse_dss([DS|Acc],DSs).

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

parse_rra([[T1,CF1],[_T2,Rows1],[_T3,Pdppr],[_T4,XFF1]|More],Step) ->
    RRA_HD=hd(string:tokens(T1,".")),
    Rows=list_to_integer(Rows1),
    PperSampl=list_to_integer(Pdppr),
    [_,RRA_Id]=string:tokens(RRA_HD,"[]"),
    RRA=#rrd_rra{id=list_to_integer(RRA_Id),
		 cf=list_to_atom(CF1),
		 duration=time:seconds_to_duration(Rows*Step*PperSampl),
		 interval=time:seconds_to_duration(Step*PperSampl),
		 xff=list_to_float(XFF1)},
    Rest=eat_cdp_prep(RRA_HD,More),
    [RRA|parse_rra(Rest,Step)];
parse_rra([],_) ->
    [].
    
eat_cdp_prep(RRA,[[T1,_Y]|More]=Input) ->
    case hd(string:tokens(T1,".")) of
	RRA ->
	    eat_cdp_prep(RRA,More);
	_Other ->
	    Input
    end;
eat_cdp_prep(_RRA,[]) ->
    [].
