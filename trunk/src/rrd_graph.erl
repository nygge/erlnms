%%%-------------------------------------------------------------------
%%% File    : rrd_graph.erl
%%% Author  : Anders Nygren <anders@local>
%%% Description : 
%%%
%%% Created : 27 Aug 2003 by Anders Nygren <anders@local>
%%%-------------------------------------------------------------------
-module(rrd_graph).

-export([do_graph/3,par_to_binary/1]).

do_graph(Port,File,Pars) ->
    CMD=create_cmd(File,Pars),
    rrd_lib:do_cmd(Port,CMD).

create_cmd(File,[{flags,Flags}|Pars]) ->
    [list_to_binary("graph "),
     list_to_binary(File), <<" ">>,
     flags_to_binary(Flags),
     pars_to_binary(Pars),
     <<$\n>>].

flags_to_binary(Flags) ->
    lists:map(fun(X)->
		      flag_to_binary(X)
	      end, Flags).

flag_to_binary({start,T}) ->
    mk_flag("s",rrd_lib_utils:datetime_to_epoch(T));
flag_to_binary({'end',T}) ->
    mk_flag("e",rrd_lib_utils:datetime_to_epoch(T));
flag_to_binary({x_grid,P}) ->
    mk_flag("x",P);
flag_to_binary({y_grid,P}) ->
    mk_flag("y",P);
flag_to_binary(alt_y_grid) ->
    mk_flag("alt-y-grid");
flag_to_binary(alt_y_mrtg) ->
    mk_flag("alt-y-mrtg");
flag_to_binary(alt_autoscale) ->
    mk_flag("alt-autoscale ");
flag_to_binary(alt_autoscale_max) ->
    mk_flag("alt-autoscale-max ");
flag_to_binary({units_exponent,V}) ->
    mk_flag("units_exponent",V);
flag_to_binary({vertical_label,T}) ->
    mk_flag("vertical_label",T);
flag_to_binary({width,W}) ->
    mk_flag("width",W);
flag_to_binary({height,H}) ->
    mk_flag("height",H);
flag_to_binary(interlaced) ->
    mk_flag("interlaced ");
flag_to_binary({imginfo,F}) ->
    mk_flag("imginfo",F);
flag_to_binary({imgformat,gif}) ->
    mk_flag("imgformat","GIF");
flag_to_binary({imgformat,png}) ->
    mk_flag("imgformat","PNG");
flag_to_binary({imgformat,gd}) ->
    mk_flag("imgformat","GD");
flag_to_binary({background,V}) ->
    mk_flag("B",V);
flag_to_binary({overlay,V}) ->
    mk_flag("O",V);
flag_to_binary({unit,V}) ->
    mk_flag("U",V);
flag_to_binary(lazy) ->
    mk_flag("z");
flag_to_binary({upper_limit,V}) ->
    mk_flag("u",V);
flag_to_binary({lower_limit,V}) ->
    mk_flag("l",V);
flag_to_binary(rigid) ->
    mk_flag("r");
flag_to_binary({base,V}) ->
    mk_flag("B",V);
flag_to_binary(logarithmic) ->
    mk_flag("o");
flag_to_binary({color,V}) ->
    mk_flag("c",V);
flag_to_binary(no_legend) ->
    mk_flag("g");
flag_to_binary({title,T}) ->
    mk_flag("t",T);
flag_to_binary({step,V}) ->
    mk_flag("step",V).

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
    
pars_to_binary(Pars) ->
    lists:map(fun(X)->
		      par_to_binary(X)
	      end, Pars).

par_to_binary({def,Defs}) ->
    lists:map(fun(X)->
		      def_to_binary(X)
	      end, Defs);

par_to_binary({cdef,CDEFs}) ->
    lists:map(fun(X)->
		      cdef_to_binary(X)
	      end, CDEFs);

par_to_binary({vdef,VDEFs}) ->
    lists:map(fun(X)->
		      vdef_to_binary(X)
	      end, VDEFs);

par_to_binary({print,Xs}) ->
    lists:map(fun(X)->
		      print_to_binary(X)
	      end, Xs);

par_to_binary({comment,Xs}) ->
    lists:map(fun(X)->
		      comment_to_binary(X)
	      end, Xs);

par_to_binary({hrule,Xs}) ->
    lists:map(fun(X)->
		     hrule_to_binary(X)
	      end, Xs);

par_to_binary({vrule,Xs}) ->
    lists:map(fun(X)->
		      vrule_to_binary(X)
	      end, Xs);

par_to_binary({line,Xs}) ->
    lists:map(fun(X)->
		      line_to_binary(X)
	      end, Xs);

par_to_binary({area,Xs}) ->
    lists:map(fun(X)->
		      area_to_binary(X)
	      end, Xs);

par_to_binary({stack,Xs}) ->
    lists:map(fun(X)->
		      stack_to_binary(X)
	      end, Xs).

def_to_binary({VName,RRD,DS,CF}) ->
    rrd_lib_utils:vals_to_binary(["DEF:",VName,"=",RRD,":",DS,":",CF," "]).

cdef_to_binary({VName,Expr}) ->
    rrd_lib_utils:vals_to_binary(["CDEF:",VName,"=",Expr," "]).

vdef_to_binary({VName,Expr}) ->
    rrd_lib_utils:vals_to_binary(["VDEF:",VName,"=",Expr," "]).

print_to_binary({VName,CF,Format})->
    [rrd_lib_utils:vals_to_binary(["PRINT",VName,CF,Format],":"),<<" ">>].

comment_to_binary(Text) ->
    [<<"COMMENT:">>,list_to_binary(Text),<<" ">>].
    %%[rrd_lib_utils:vals_to_binary(["COMMENT",Text],":"),<<" ">>].

hrule_to_binary(Val) ->
    [rrd_lib_utils:vals_to_binary(["HRULE",Val],":"),<<" ">>].

vrule_to_binary(Val) ->
    [rrd_lib_utils:vals_to_binary(["VRULE",Val],":"),<<" ">>].

line_to_binary({LNo,More}) ->
    [rrd_lib_utils:vals_to_binary(["LINE"++integer_to_list(LNo),More],":"),
     <<" ">>].

area_to_binary(Pars) ->
    rrd_lib_utils:vals_to_binary(["AREA"|Pars],":").

stack_to_binary(Pars) ->
    rrd_lib_utils:vals_to_binary(["STACK"|Pars],":").
