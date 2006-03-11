%%%-------------------------------------------------------------------
%%% File    : rrd_xport.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 30 Jan 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_xport).

-export([do_xport/2]).

%%%-------------------------------------------------------------------
%%% do_xport(Port,Pars)
%%% Port = port(),
%%% Pars = {Flags,DEFs,CDEFs,XPORTs}
%%%
%%% Flags = [Flag]
%%% Flag= {start,Start}|{'end',End}|{maxrows,MaxRows}|{step,Step}
%%% Start = End = datetime()
%%% MaxRows = int()
%%% DEFs = [DEF]
%%% CDEFs = [CDEF]
%%% XPORTs = [XPORT]
%%%
%%% DEF = {Vname,RRD,DS_name,CF}
%%% Vname = atom()
%%% RRD = string()
%%% DS_name = atom()
%%% CF = 'AVERAGE'|'MIN'|'MAX'|'LAST'
%%%
%%% CDEF = {Vname,RPN}
%%% RPN = string()
%%%
%%% XPORT = {Vname,Legend}
%%% Legend = string()

do_xport(Port,{Flags,DEFs,CDEFs,XPORTs}) ->
    CMD=create_cmd(Flags,DEFs,CDEFs,XPORTs),
    {_,File,_,_}=hd(DEFs),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,Result} ->
	    parse_res(Result);
	Error ->
	    Error
    end.

create_cmd(Flags,DEFs,CDEFs,XPORTs) ->
    [<<"xport ">>,
     flags_to_binary(Flags),
     rrd_graph:par_to_binary({def,DEFs}),
     rrd_graph:par_to_binary({cdef,CDEFs}),
     xports_to_binary(XPORTs),
     <<$\n>>].

flags_to_binary(Flags) ->
    lists:map(fun (X) ->
		      flag_to_binary(X)
	      end,
	      Flags).

flag_to_binary({start,T}) ->
    mk_flag("s",utils:datetime_to_epoch(T));
flag_to_binary({'end',T}) ->
    mk_flag("e",utils:datetime_to_epoch(T));
flag_to_binary({maxrows,P}) ->
    mk_flag("m",P);
flag_to_binary({step,P}) ->
    mk_flag("step",utils:duration_to_seconds(P)).

mk_flag(Flag,Value) ->
    Sign=case length(Flag) of
	     1 ->
		 <<"-">>;
	     N ->
		 <<"--">>
		     end,
    [Sign,list_to_binary(Flag),<<" ">>,
     rrd_lib_utils:val_to_binary(Value),
     <<" ">>].

xports_to_binary(XPORTs) ->
    lists:map(fun(X) ->
		      xport_to_binary(X)
	      end,
	      XPORTs).

xport_to_binary({Vname,Legend}) ->
    rrd_lib_utils:vals_to_binary(["XPORT:",Vname,":",Legend," "]).
   

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
			 {utils:epoch_to_datetime(list_to_integer(TS)),
			  cnt_to_val(Ds)}
		 end,
		 Data),
    {Meta,D1}.

parse_meta([("<start>"++M)=Line|More]) ->
    R={start,utils:epoch_to_datetime(parse_line_int(Line))},
    [R|parse_meta(More)];
parse_meta([("<end>"++M)=Line|More]) ->
    R={'end',utils:epoch_to_datetime(parse_line_int(Line))},
    [R|parse_meta(More)];
parse_meta([("<step>"++M)=Line|More]) ->
    R={'step',utils:seconds_to_duration(parse_line_int(Line))},
    [R|parse_meta(More)];
parse_meta([("<rows>"++M)=Line|More]) ->
    R={rows,parse_line_int(Line)},
    [R|parse_meta(More)];
parse_meta([("<columns>"++M)=Line|More]) ->
    R={columns,parse_line_int(Line)},
    [R|parse_meta(More)];
parse_meta([("<entry>"++M)=Line|More]=L) ->
    {Es,Rest}=parse_entry([],L),
    [Es|parse_meta(Rest)];
parse_meta([X|Y]) ->
    parse_meta(Y);
parse_meta([]) ->
    [].

parse_line_int(Line) ->
    list_to_integer(lists:nth(2,string:tokens(Line,"<>/"))).

parse_entry(Acc,[("<entry>"++M)=Line|More]) ->
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
			  {'EXIT',Reason} ->
			      list_to_integer(X);
			  F ->
			      F
		      end
	      end,
	      Ds).
