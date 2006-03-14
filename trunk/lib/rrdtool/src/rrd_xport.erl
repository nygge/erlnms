%%%-------------------------------------------------------------------
%%% File    : rrd_xport.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 30 Jan 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_xport).

-export([do_xport/2]).

-include("rrdtool.hrl").

%%%-------------------------------------------------------------------
%%% do_xport(Port,Pars)
%%% Port = port(),

do_xport(Port,Def) ->
    CMD=create_cmd(Def),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,Result} ->
	    parse_res(Result);
	Error ->
	    Error
    end.

create_cmd(Def) ->
    [<<"xport ">>,
     start_to_binary(Def#rrd_export.start),
     stop_to_binary(Def#rrd_export.stop),
     rows_to_binary(Def#rrd_export.rows),
     step_to_binary(Def#rrd_export.step),
     rrd_graph:defs_to_binary(Def#rrd_export.defs),
     rrd_graph:cdefs_to_binary(Def#rrd_export.cdefs),
     xports_to_binary(Def#rrd_export.xports),
     <<$\n>>].

start_to_binary(undefined) ->
    <<>>;
start_to_binary(T) ->
    mk_flag("s",time:datetime_to_epoch(T)).

stop_to_binary(undefined) ->
    <<>>;
stop_to_binary(T) ->
    mk_flag("e",time:datetime_to_epoch(T)).

rows_to_binary(undefined) ->
    <<>>;
rows_to_binary(R) ->
    mk_flag("m",R).

step_to_binary(undefined) ->
    <<>>;
step_to_binary(S) ->
    mk_flag("step",time:duration_to_seconds(S)).

mk_flag(Flag,Value) ->
    Sign=case length(Flag) of
	     1 ->
		 <<"-">>;
	     _N ->
		 <<"--">>
		     end,
    [Sign,list_to_binary(Flag),<<" ">>,
     rrd_lib_utils:val_to_binary(Value),
     <<" ">>].

xports_to_binary(XPORTs) ->
    lists:map(fun(#rrd_xport{vname=Vname,legend=Legend}) ->
		      rrd_lib_utils:vals_to_binary(["XPORT:",Vname,
						    ":",Legend," "])
	      end,
	      XPORTs).

%%%========================================================================
%%%
%%%   Parse result
%%%

parse_res(Res) ->
    Tokens=string:tokens(Res," \n"),
    {M,D}=lists:splitwith(fun (X)->
				     X/="<data>"
			     end,
			     Tokens),
    Meta=parse_meta(M),
    DL=length(D),
    Data=lists:sublist(D,2,DL-3),
    D1=lists:map(fun(R) ->
			 [TS|Ds]=string:tokens(R,"<>/tvrow"),
			 {time:epoch_to_datetime(list_to_integer(TS)),
			  cnt_to_val(Ds)}
		 end,
		 Data),
    {Meta,D1}.

parse_meta([("<start>"++_M)=Line|More]) ->
    R={start,time:epoch_to_datetime(parse_line_int(Line))},
    [R|parse_meta(More)];
parse_meta([("<end>"++_M)=Line|More]) ->
    R={'end',time:epoch_to_datetime(parse_line_int(Line))},
    [R|parse_meta(More)];
parse_meta([("<step>"++_M)=Line|More]) ->
    R={'step',time:seconds_to_duration(parse_line_int(Line))},
    [R|parse_meta(More)];
parse_meta([("<rows>"++_M)=Line|More]) ->
    R={rows,parse_line_int(Line)},
    [R|parse_meta(More)];
parse_meta([("<columns>"++_M)=Line|More]) ->
    R={columns,parse_line_int(Line)},
    [R|parse_meta(More)];
parse_meta([("<entry>"++_M)|_More]=L) ->
    {Es,Rest}=parse_entry([],L),
    [Es|parse_meta(Rest)];
parse_meta([_X|Y]) ->
    parse_meta(Y);
parse_meta([]) ->
    [].

parse_line_int(Line) ->
    list_to_integer(lists:nth(2,string:tokens(Line,"<>/"))).

parse_entry(Acc,[("<entry>"++_M)=Line|More]) ->
    R=parse_line_atom(Line),
    parse_entry([R|Acc],More);
parse_entry(Acc,X) ->
    {{legends,lists:reverse(Acc)},X}.

parse_line_atom(Line) ->
    list_to_atom(lists:nth(2,string:tokens(Line,"<>/"))).


cnt_to_val(Ds) ->
    lists:map(fun ("NaN") ->
		      unknown;
		  (X) ->
		      case catch list_to_float(X) of
			  {'EXIT',_Reason} ->
			      list_to_integer(X);
			  F ->
			      F
		      end
	      end,
	      Ds).
