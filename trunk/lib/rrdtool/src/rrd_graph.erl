%%%-------------------------------------------------------------------
%%% File    : rrd_graph.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 27 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_graph).

-export([do_graph/2,
	 defs_to_binary/1,
	 cdefs_to_binary/1]).

-include("rrdtool.hrl").

do_graph(Port,Pars) ->
    CMD=create_cmd(Pars),
    io:format("~p~n",[binary_to_list(list_to_binary(lists:flatten(CMD)))]),
    rrd_lib:do_cmd(Port,CMD).

create_cmd(Pars) ->
    [list_to_binary("graph "),
     list_to_binary(Pars#rrd_graph.file), <<" ">>,
     start_to_binary(Pars#rrd_graph.start),
     stop_to_binary(Pars#rrd_graph.stop),
     step_to_binary(Pars#rrd_graph.step),
     flags_to_binary(Pars#rrd_graph.options),
     defs_to_binary(Pars#rrd_graph.defs),
     cdefs_to_binary(Pars#rrd_graph.cdefs),
     vdefs_to_binary(Pars#rrd_graph.vdefs),
     graph_to_binary(Pars#rrd_graph.graph),
     <<$\n>>].

flags_to_binary(Flags) ->
    lists:map(fun(X)->
		      flag_to_binary(X)
	      end, Flags).

start_to_binary(undefined) ->
    <<>>;
start_to_binary(T) ->
    mk_flag("s",time:datetime_to_epoch(T)).

stop_to_binary(undefined) ->
    <<>>;
stop_to_binary(T) ->
    mk_flag("e",time:datetime_to_epoch(T)).

step_to_binary(undefined) ->
    <<>>;
step_to_binary(V) ->
    mk_flag("S",time:duration_to_seconds(V)).

flag_to_binary(alt_autoscale) ->
    mk_flag("alt-autoscale ");
flag_to_binary(alt_autoscale_max) ->
    mk_flag("alt-autoscale-max ");
flag_to_binary(alt_y_grid) ->
    mk_flag("alt-y-grid");
flag_to_binary(alt_y_mrtg) ->
    mk_flag("alt-y-mrtg");
flag_to_binary({background,V}) ->
    mk_flag("B",V);
flag_to_binary({base,V}) ->
    mk_flag("B",V);
flag_to_binary({color,V}) ->
    mk_flag("c",V);
flag_to_binary({height,H}) ->
    mk_flag("height",H);
flag_to_binary({imginfo,F}) ->
    mk_flag("imginfo",F);
flag_to_binary({imgformat,gif}) ->
    mk_flag("imgformat","GIF");
flag_to_binary({imgformat,png}) ->
    mk_flag("imgformat","PNG");
flag_to_binary({imgformat,gd}) ->
    mk_flag("imgformat","GD");
flag_to_binary(interlaced) ->
    mk_flag("interlaced ");
flag_to_binary(lazy) ->
    mk_flag("z");
flag_to_binary(logarithmic) ->
    mk_flag("o");
flag_to_binary({lower_limit,V}) ->
    mk_flag("l",V);
flag_to_binary(no_legend) ->
    mk_flag("g");
flag_to_binary({overlay,V}) ->
    mk_flag("O",V);
flag_to_binary(rigid) ->
    mk_flag("r");
flag_to_binary({title,T}) ->
    mk_flag("t",T);
flag_to_binary({unit,V}) ->
    mk_flag("U",V);
flag_to_binary({units_exponent,V}) ->
    mk_flag("units_exponent",V);
flag_to_binary({upper_limit,V}) ->
    mk_flag("u",V);
flag_to_binary({vertical_label,T}) ->
    mk_flag("vertical_label",T);
flag_to_binary({width,W}) ->
    mk_flag("width",W);
flag_to_binary({x_grid,P}) ->
    mk_flag("x",P);
flag_to_binary({y_grid,P}) ->
    mk_flag("y",P).

mk_flag(Flag) ->
    Sign=case length(Flag) of
	     1 ->
		 <<"-">>;
	     _N ->
		 <<"--">>
		     end,
    [Sign,list_to_binary(Flag),<<" ">>].

mk_flag(Flag,Value) when is_list(Value)->
    [<<"-">>,
     list_to_binary(Flag),
     <<" ">>,<<$">>,
     rrd_lib_utils:val_to_binary(Value),
     <<$">>,<<" ">>];
    
mk_flag(Flag,Value) ->
    [<<"-">>,
     list_to_binary(Flag),
     <<" ">>,
     rrd_lib_utils:val_to_binary(Value),
     <<" ">>].

defs_to_binary(Defs) ->
    lists:map(fun(#rrd_def{vname=VName,rrd=RRD,ds_name=DS,cf=CF})->
		      rrd_lib_utils:vals_to_binary(["DEF:",VName,
						    "=",RRD,":",DS,
						    ":",CF," "])
	      end, Defs).

cdefs_to_binary(CDEFs) ->
    lists:map(fun(#rrd_cdef{vname=VName,rpn=RPN})->
		      rrd_lib_utils:vals_to_binary(["CDEF:",VName,
						    "=",RPN," "])
	      end, CDEFs).

vdefs_to_binary(VDEFs) ->
    lists:map(fun(#rrd_vdef{vname=VName,rpn=RPN})->
		      rrd_lib_utils:vals_to_binary(["VDEF:",VName,
						    "=",RPN," "])
	      end, VDEFs).

graph_to_binary(Ds) ->
    lists:map(fun (D) ->
		      g_to_bin(D)
	      end,Ds).

g_to_bin(#rrd_area{vname=V,color=_C,legend=_L,stack=_S}) ->
    rrd_lib_utils:vals_to_binary(["AREA",V],":");

g_to_bin(#rrd_comment{text=Text}) ->
    [<<"COMMENT:">>,list_to_binary(Text),<<" ">>];

g_to_bin(#rrd_line{width=W,vname=V,color=undefined,legend=L,stack=S}) ->
    LS=mk_leg_stack(L,S),
    More=[V|LS],
    [rrd_lib_utils:vals_to_binary(["LINE"++integer_to_list(W)|More],":")
     ,<<" ">>];
g_to_bin(#rrd_line{width=W,vname=V,color=C,legend=L,stack=S}) ->
    LS=mk_leg_stack(L,S),
    VC1=concat_color(V,C),
    More=[VC1|LS],
    [rrd_lib_utils:vals_to_binary(["LINE"++integer_to_list(W)|More],":")
     ,<<" ">>];

g_to_bin(#rrd_print{vname=VName,format=Format})->
    [rrd_lib_utils:vals_to_binary(["PRINT",VName,Format],":"),<<" ">>];

g_to_bin(#rrd_shift{vname=V,offset=Off}) ->
    rrd_lib_utils:vals_to_binary(["SHIFT",V,Off],":");

g_to_bin(#rrd_tick{vname=V,rrggbb=_RGB,aa=_AA,fraction=_F,legend=_L}) ->
    rrd_lib_utils:vals_to_binary(["TICK",V],":");

g_to_bin(#rrd_vrule{time=Time,color=Color,legend=undefined}) ->
    VC1=concat_color(Time,Color),
    [rrd_lib_utils:vals_to_binary(["VRULE",VC1],":"),<<" ">>];
g_to_bin(#rrd_vrule{time=Time,color=Color,legend=L}) ->
    VC1=concat_color(Time,Color),
    [rrd_lib_utils:vals_to_binary(["VRULE",VC1,L],":"),<<" ">>].

mk_leg_stack(undefined,undefined) ->
    [];
mk_leg_stack(undefined,true) ->
    ["STACK"];
mk_leg_stack(Legend,undefined) ->
    [Legend];
mk_leg_stack(Legend,true) ->
    [Legend,"STACK"].

concat_color(V,C) ->
    C1=lists:flatten(io_lib:format("~.16B",[C])),
    Fill=lists:duplicate(6-length(C1),$0),
    io_lib:format("~s#~s~s",[V,Fill,C1]).
