%%%-------------------------------------------------------------------
%%% File    : rrd_lib.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : Port to rrdtool
%%%
%%% Created : 26 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------

%%% @doc RRDTool interface library.
%%%
%%% <p>This is an Erlang library that provides an interface to RRDTool,
%%% the Round Robin Database.</p>
%%% <p>For details on how to use RRDTool see the 
%%% <a href="http://somelink.com.ch">RRDTool documentation</a></p>
%%%
%%% @copyright 2005 Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @version 0.8, {10 Apr 2005}
%%% @end

-module(rrd_lib).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% External exports
-export([open/0,close/1,do_cmd/2,
	 create/2,
%%	 dump/1,
	 fetch/4,fetch/6,
	 graph/3,
	 info/2,info/3,
	 last/2,
	 restore/3,restore/4,
%%	 rrdresize/4,
%%	 tune/2,
	 update/3,update/4,
	 xport/2 
	]).
 
%%====================================================================
%% External functions
%%====================================================================

%% @spec open() -> Result
%% Result = {ok,port()} | {error,PosixError}
%%
%% @doc Opens a connection to an instance of RRDTool.
%%
%%
open() ->
    Cmd="/usr/bin/rrdtool -",
%%    Cmd="/usr/bin/rrdtool -",
    case catch open_port({spawn,Cmd},[stream]) of
	Port when is_port(Port) ->
	    %%process_flag(trap_exit, true),		  
	    link(Port),
	    {ok, Port};
	{'EXIT',_Port,PosixCode} ->
	    {error,PosixCode}
    end.

%% @spec close(Port) -> true
%%
%% @doc Close a port to an instance of RRDTool.
%%
%% <p> Failure: badarg if Port is not an open port.</p>
%%
close(Port) when is_port(Port) ->
    port_close(Port).

%% @spec create(Port,RRDSpec) -> Result
%%
%%
%% Result = {ok,nothing} | {error,Reason}
%%
%% @doc Create a RRDTool database file.
%%
%% <p> For a more detailed description of the parameters see the
%% RRDTool <a href="http://......">documentation</a>.</p>
%%

create(Port,RRDSpec) when is_port(Port) ->
    rrd_create:do_create(Port,RRDSpec).

%% %% @spec dump(Port,File,ToFile) -> Result
%% %% Port    = port()
%% %% File    = string()
%% %% ToFile  = string()
%% %% Result  = WHAT
%% %% @doc Dump a RRDTool database to an XML-file
%% %%

%% dump(Port,File,ToFile) ->
%%     rrd_dump:dump(Port,File,ToFile}).

%% @spec fetch(Port,File,CF,Res) -> Result
%%
%% Port       = port()
%% File       = string()
%% CF         = 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'
%% Res        = {Unit, integer()}
%% Unit       = sec | min | hour | day | week | month | year
%% Result     = WHAT
%%
%% @doc Fetch data from an RRDTool database file
%%
%% @equiv fetch(Port,File,CF,Res,latest,latest)
%%

fetch(Port,File,CF,Res) when is_port(Port) ->
    rrd_fetch:do_fetch(Port,File,CF,Res,latest,latest).

%% @spec fetch(Port,File,CF,Res,Start,Stop) -> Result
%%
%% Port       = port()
%% File       = string()
%% CF         = 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'
%% Res        = {Unit, integer()}
%% Unit       = sec | min | hour | day | week | month | year
%% Start      = DateTime | latest
%% Stop       = DateTime | latest
%% DateTime   = {{YYYY,MM,DD},{HH,MM,SS}}
%% Result     = WHAT
%%
%% @doc Fetch data from an RRDTool database file
%%

fetch(Port,File,CF,Res,Start,Stop) when is_port(Port) ->
    rrd_fetch:do_fetch(Port,File,CF,Res,Start,Stop).

%% @spec graph(Port,File,Pars) -> Result
%%
%% File  = string()
%% Pars  = [Flags|Ps]
%% Flags = [Flag]
%% Flag  = {start,T} | {'end',T} | {x_grid,P} | {y_grid,P} | alt_y_grid | 
%%         alt_y_mrtg | alt_autoscale | alt_autoscale_max | 
%%         {units_exponent,V} | {vertical_label,V} | {width,W} | 
%%         {height,H} | interlaced | {imginfo,F} | {imgformat,F} |
%%         {background,V} | {overlay,V} | {unit,V} | lazy | {upper_limit,V} |
%%         {lowerlimit,V} | rigid | {base,V} | logarithmic | {color,V} | 
%%         no_legend | {title,T} | {step,V}
%%
%% Ps    = [P]
%% P     = {def,DEFs} | {cdef,CDEFs} | {print,PRINTs} | {comment, COMMENTs} | 
%%         {hrule,HRULEs} | {vrule,VRULEs} | {line,LINEs} | {area,AREAs} | 
%%         {stack,STACKs}
%%
%% DEFs  = [DEF]
%% DEF   = {Vname,RRD,DS,CF}
%% VName = string
%% RRD   = string
%% DS    = atom
%% CF    = 'MAX'|'MIN'|'LAST'|'AVERAGE'
%%
%% CDEFs = [CDEF]
%% CDEF  = {Vname,Expr}
%%
%% VDEFs = [VDEF]
%% VDEF  = {Vname,Expr}
%%
%% VName = string
%% Expr  = string
%%
%% PRINTs= [PRINT]
%% PRINT = {VName,CF,Format}
%% VName = string
%% CF    = 'MAX'|'MIN'|'LAST'|'AVERAGE'
%% Format= string
%%
%% GPRINTs= [GPRINT]
%% GPRINT = {VName,CF,Format}
%% VName  = string
%% CF     = 'MAX'|'MIN'|'LAST'|'AVERAGE'
%% Format =  string
%%
%% COMMENTs = [COMMENT]
%% COMMENT  = string
%%
%% HRULEs = [HRULE]
%% HRULE  = {Val,Color} | {Val,Color,Legend}
%% Val    = number
%% Color  = number
%% Legend = string
%%
%% VRULEs = [VRULE]
%% VRULE  = {Val,Color} | {Val,Color,Legend}
%% Val    = number
%% Color  = number
%% Legend = string
%%
%% LINEs  = [LINE]
%% LINE   = {Type,VName} | {Type,VName,Color} | {Type,VName,Color,Legend}
%% Type   = 1|2|3
%% Val    = number
%% Color  = number
%% Legend = string
%%
%% AREAs  = [AREA]
%% AREA   = VName | {VName,Color} | {VName,Color,Legend}
%% VName  = string
%% Color  = number
%% Legend = string
%%
%% STACKs = [STACK]
%% STACK  = VName | {VName,Color} | {VName,Color,Legend}
%% VName  = string
%% Color  = number
%% Legend = string
%% Result = {ok, nothing} | {error, Reason}
%%
%% @doc Create a graph.
%%
%% <p> For a more detailed description of the parameters see the
%% RRDTool <a href="http://......">documentation</a>.</p>
 
graph(Port,File,Pars) when is_port(Port) ->
    rrd_graph:do_graph(Port,File,Pars).

%% @spec info(Port,File) -> Result
%% Port      = port()
%% File      = string()
%% Result    = WHAT
%%
%% @doc Get information about a RRDTool database
%%
%% @equiv info(Port,File,all)

info(Port,File) when is_port(Port) ->
    rrd_info:do_info(Port,File,all).

%% @spec info(Port,File,Type) -> Result
%% Port      = port()
%% File      = string()
%% Type      = all | last | dss | rra
%% Result    = WHAT
%%
%% @doc Get information about a RRDTool database.
%%
%% <p>When Type=all, returns all information about the database.</p>
%% <p>When Type=last, returns information about the last time the database was updated.</p>
%% <p>When Type=dss, returns information about the datasources in the database.</p>
%% <p>When Type=rra, returns information about the roundrobin archives in the database.</p>

info(Port,File,Type) when is_port(Port), Type==all; Type==last; Type==dss; Type==rra ->
    rrd_info:do_info(Port,File,Type).

%% @spec last(Port,File) -> Result
%% Port     = port()
%% File     = string()
%% Result   = WHAT
%%
%% @doc Get the timestamp for the last update of a database.
%%

last(Port,File) when is_port(Port) ->
    rrd_last:do_last(Port,File).

%% @spec restore(Port,FromFile,ToFile) -> Result
%% Port     = port()
%% FromFile = string()
%% ToFile   = string()
%% Result   = WHAT
%%
%% @doc Restore the contents of an RRD from an XML dump
%%
%% @equiv restore(Port,[],FromFile,ToFile)

restore(Port,FromFile,ToFile) when is_port(Port) ->
    restore(Port,[],FromFile,ToFile).

%% @spec restore(Port,Opts,FromFile,ToFile) -> Result
%% Port     = port()
%% Opts     = [Opt]
%% Opt      = range_check
%% FromFile = string()
%% ToFile   = string()
%% Result   = WHAT
%%
%% @doc Restore the contents of an RRD from an XML dump
%%

restore(Port,Opts,FromFile,ToFile) when is_port(Port) ->
    rrd_restore:do_restore(Port,Opts,FromFile,ToFile).

%% %% @spec resize(Port,File,RRA,OP,Rows) -> Result
%% %% Port     = port()
%% %% File     = string()
%% %% RRA      = atom()
%% %% OP       = grow | shrink
%% %% Rows     = integer()
%% %%
%% %% @doc Add or delete rows in an RRDTool database.
%% %%

%% resize(Port,File,RRA,OP,Rows) ->
%%     rrd_resize:do_resize(File,RRA,OP,Rows}).

%% tune(Port,File,Pars) ->
%%     rrd_tune:do_tune(File,Pars}).

%% @spec update(Port,File,Values) -> Result
%% Port     = port()
%% File     = string()
%% ValueSets = [ValueSet]
%% ValueSet  = {n,[Value]} | {TS,[Value]}

%% Result   = ok | {error, Reason}
%% Reason = string()
%%
%% @doc Updates an RRD.
%%
%% @equiv update(Port,File,[],Values)

update(Port,File,Values) when is_port(Port), is_list(Values) ->
    update(Port,File,[],Values).

%% @spec update(Port,File,Template,ValueSets) -> Result
%% Port      = port()
%% File      = string()
%% Template  = [atom()]
%% ValueSets = [ValueSet]
%% ValueSet  = {n,[Value]} | {TS,[Value]}
%% Result    = {ok,TSs} | {error, Reason}
%% TSs       = [TS]
%% TS        = integer()
%% Reason    = string()
%%
%% @doc Updates an RRD.
%%
%% 

update(Port,File,Template,Values) when is_port(Port),
				       is_list(File),
				       is_list(Template), 
				       is_list(Values) ->
    rrd_update:do_update(Port,File,Template,Values).

%% @spec xport(Port,Pars) -> Result
%% Port    = port()
%% Pars    = {Flags,DEFs,CDEFs,XPORTs}
%% Flags   = [Flag]
%% Flag    = {start,Start}|{'end',End}|{maxrows,MaxRows}|{step,Step}
%% Start   = datetime()
%% End     = datetime()
%% MaxRows = int()
%% DEFs    = [DEF]
%% CDEFs   = [CDEF]
%% XPORTs  = [XPORT]
%%
%% DEF     = {Vname,RRD,DS_name,CF}
%% Vname   = atom()
%% RRD     = string()
%% DS_name = atom()
%% CF      = 'AVERAGE'|'MIN'|'MAX'|'LAST'
%%
%% CDEF    = {Vname,RPN}
%% RPN     = string()
%%
%% XPORT   = {Vname,Legend}
%% Legend  = string()
%%
%% @doc Get data from RRDTool database.

xport(Port,Pars) when is_port(Port) ->
    rrd_xport:do_xport(Port,Pars).

%% @private
do_cmd(Port,CMD) when is_port(Port) ->
%%    io:format("rrd_lib=~p~n",[CMD]),
    case catch port_command(Port,CMD) of
	{'EXIT',Port,Reason} ->
  	    {error,Reason};
  	true ->
  	    receive
  		{Port,{data,Res}} ->
		    io:format("Resp=~p~n",[Res]),
   		    parse_resp(Res)
	    end
    end.

parse_resp("ERROR:"++More) ->
    Res=string:left(More,string:str(More,"\nOK")-1),
    {error,Res};
parse_resp("-1\nERROR:"++More) ->
    Res=string:left(More,string:str(More,"\nOK")-1),
    {error,Res};
parse_resp("OK"++_More) ->
    {ok,nothing};
parse_resp(Resp) ->
    Res=string:left(Resp,string:str(Resp,"\nOK")-1),
    {ok,Res}.
