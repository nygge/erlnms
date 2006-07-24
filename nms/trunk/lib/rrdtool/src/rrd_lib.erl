%%%-------------------------------------------------------------------
%%% File    : rrd_lib.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2003-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc RRDTool interface library.
%%%
%%% <p>This is an Erlang library that provides an interface to RRDTool,
%%% the Round Robin Database.</p>
%%% <p>For details on how to use RRDTool see the 
%%% <a href="http://somelink.com.ch">RRDTool documentation</a></p>
%%%
%%% @end
%%% @type cf() = 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'. 
%%% Consolidation function, defines how datapoints are consolidated into a new datapoint with a longer time interval.
%%%
%%% @type ds_type() = 'GAUGE' | 'COUNTER' | 'DERIVE' | 'ABSOLUTE'.
%%% Type of data source.
%%%
%%% %@type duration() = {unit(),integer()}. 
%%% %Denotes a time interval.
%%%
%%% @type heartbeat() = //utils/time:duration().
%%% Max number of seconds between two updates.
%%%
%%% options() = [option()].
%%% option()  = {start,T} | {'end',T} | {x_grid,P} | {y_grid,P} | 
%%%             alt_y_grid | alt_y_mrtg | alt_autoscale | 
%%%             alt_autoscale_max | {units_exponent,V} | 
%%%             {vertical_label,V} | {width,W} | {height,H} | 
%%%             interlaced | {imginfo,F} | {imgformat,F} | {background,V} | 
%%%             {overlay,V} | {unit,V} | lazy | {upper_limit,V} |
%%%             {lowerlimit,V} | rigid | {base,V} | logarithmic | {color,V} | 
%%%             no_legend | {title,T} | {step,V}.
%%%
%%% @type rrd_ds_type() = rrd_ds()|rrd_ds_comp().
%%% Datasource type.
%%%
%%% @type rrd_ds() = Record.
%%% <pre>
%%% -record(rrd_ds,{name::string(),
%%%                 type::ds_type(),
%%%                 hb::heartbeat(),
%%%                 min::number(),
%%%                 max::number()}).
%%% </pre>
%%% A normal datasource.
%%%
%%% @type rrd_ds_comp() = Record.
%%% <pre>
%%% -record(rrd_ds_comp,{name::string(),
%%%                      rpn::string()}).
%%% </pre>
%%% A computed datasource.
%%%
%%% @type rrd_export() = Record.
%%% <pre>
%%% -record(rrd_export,{start:://utils/time:datetime(),
%%%                     stop:://utils/time:datetime(),
%%%                     rows::integer,
%%%                     step:://utils/time:duration(),
%%%                     defs::[rrd_def()],
%%%                     cdefs::[rrd_cdef()],
%%%                     exports::[rrd_xport()]}).
%%% </pre>
%%% 
%%% @type rrd_file()=Record.
%%% <pre>
%%% -record(rrd_file,{file::string(),
%%%                   start:://utils/time:duration(),
%%%                   step:://utils/time:duration(),
%%%                   dss::[rrd_ds_type()],
%%%                   rras[rrd_rra_type()]}).
%%% </pre>
%%% Defines a rrdfile.
%%%
%%% @type rrd_graph() = Record.
%%% <pre>
%%% -record(rrd_graph,{file::string(),
%%%                    start:://utils/time:datetime(),
%%%                    stop:://utils/time:datetime(),
%%%                    step:://utils/time:duration(),
%%%                    defs::[rrd_def()],
%%%                    cdefs::[rrd_cdef()],
%%%                    vdefs::[rrd_vdef()],
%%%                    graph::[graph_opt()],
%%%                    options::[option()]}).
%%% </pre>
%%%
%%% @type graph_opt() = rrd_print()|rrd_gprint()|rrd_comment()|rrd_vrule()|rrd_line()|rrd_area()|rrd_tick()|rrd_shift()
%%% @type rrd_print() = Record.
%%% <pre>
%%% -record(rrd_print,{vname::string(),format::string()}).
%%% </pre>
%%% 
%%% @type rrd_gprint()  = Record.
%%% <pre>
%%% -record(rrd_gprint,{vname::string(),format::format()}).
%%% </pre>
%%% 
%%% @type rrd_comment()  = Record.
%%% <pre>
%%% -record(rrd_comment,{text::string()}).
%%% </pre>
%%% 
%%% @type rrd_vrule() = Record.
%%% <pre>
%%% -record(rrd_vrule,{time,color,legend}).
%%% </pre>
%%% 
%%% @type rrd_line() = Record.
%%% <pre>
%%% -record(rrd_line,{width,vname,color,legend,stack}).
%%% </pre>
%%% 
%%% @type rrd_area()  = Record.
%%% <pre>
%%% -record(rrd_area,{vname,color,legend,stack}).
%%% </pre>
%%% 
%%% @type rrd_tick()  = Record.
%%% <pre>
%%% -record(rrd_tick,{vname,rrggbb,aa,fraction,legend}).
%%% </pre>
%%% @type rrd_shift()  = Record.
%%% <pre>
%%% -record(rrd_shift,{vname,offset}).
%%% </pre>

%%% @type rrd_rra_type() = rrd_rra()|rrd_rra_hwpred()|rrd_rra_seasonal()|rrd_rra_devseason()|rrd_rra_devpredict()|rrd_rra_failures().
%%% RRA type.
%%%
%%% @type rrd_rra() = Record.
%%% <pre>
%%% -record(rrd_rra,{id::string(),
%%%                  cf::cf(),
%%%                  xff::xff(),
%%%                  interval::interval(),
%%%                  duration:://utils/time:duration()}).
%%% </pre>
%%% Normal RRA.
%%%
%%% @type rrd_rra_hwpred() = Record.
%%% <pre>
%%% -record(rrd_rra_hwpred,{id::string(),
%%%                         rows::integer(),
%%%                         alpha::number(),
%%%                         beta::number(),
%%%                         period:://utils/time:duration(),
%%%                         rra_num::integer()}).
%%% </pre>
%%%
%%% @type rrd_rra_seasonal() = Record.
%%% <pre>
%%% -record(rrd_rra_seasonal,{id::string(),
%%%                           period::number(),
%%%                           gamma::number(),
%%%                           rra_num::integer()}).
%%% </pre>
%%%
%%% @type rrd_rra_devseason() = Record.
%%% <pre>
%%% -record(rrd_rra_devseason,{id::string(),
%%%                            period::number(),
%%%                            gamma::number(),
%%%                            rra_num::integer()}).
%%% </pre>
%%%
%%% @type rrd_rra_devpredict() = Record.
%%% <pre>
%%% -record(rrd_rra_devpredict,{id::string(),
%%%                             rows::integer(),
%%%                             rra_num::integer()}).
%%% </pre>
%%%
%%% @type rrd_rra_failures() = Record.
%%% <pre>
%%% -record(rrd_rra_failures,{id::string(),
%%%                           rows::integer(),
%%%                           threshold::number(),
%%%                           window::number(),
%%%                           rra_num::integer()}).
%%% </pre>
%%%
%%% @type rrd_def() = Record.
%%% <pre>
%%% -record(rrd_def,{vname::string(),      % Variable name
%%%  		     rrd::string(),        % RRD filename
%%%		     ds_name::string(),    % Datasource name
%%%		     cf::cf()              % Consolidation functions
%%%	    }).
%%% </pre>
%%%
%%% @type rrd_cdef() = Record.
%%% <pre>
%%% -record(rrd_cdef,{vname::string(),     % Variable name
%%%		      rpn::string()        % RPN expression
%%%	   }).
%%% </pre>
%%%
%%% @type rrd_vdef() = Record.
%%% <pre>
%%% -record(rrd_vdef,{vname::string(),     % Variable name
%%%		      rpn::string()        % RPN expression
%%%		 }).
%%% </pre>
%%%
%%% @type rrd_xport() = Record.
%%% <pre>
%%% -record(rrd_xport,{vname::string(),    % Variable name
%%%		       legend::string()    % Exported name
%%%		  }).
%%% </pre>
%%%
%%% @type unit() = sec|min|hour|day|week|month|year.
%%% @end
%%% Created : 26 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------

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
	 graph/2,
	 info/2,info/3,
	 last/2,
	 restore/3,restore/4,
%%	 rrdresize/4,
%%	 tune/2,
	 update/3,update/4,
	 xport/2 
	]).

-include("rrdtool.hrl").
 
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
    Cmd="/usr/pkg/bin/rrdtool -",
%%    Cmd="/usr/bin/rrdtool -",
    case catch open_port({spawn,Cmd},[stream]) of
	Port when is_port(Port) ->
	    %%process_flag(trap_exit, true),		  
	    link(Port),
	    {ok, Port};
	{'EXIT',_Port,PosixCode} ->
	    {error,PosixCode}
    end.

%% @spec close(Port::port()) -> true
%%
%% @doc Close a port to an instance of RRDTool.
%%
%% <p> Failure: badarg if Port is not an open port.</p>
%%
close(Port) when is_port(Port) ->
    port_close(Port).

%% @spec create(Port::port(),RRDSpec::rrd_file()) -> Result
%%
%% Result = {ok,nothing} | {error,Reason}
%%
%% @doc Create a RRDTool database file.
%%
%% <p> For a more detailed description of the parameters see the
%% RRDTool <a href="http://......">documentation</a>.</p>
%%

create(Port,RRDSpec) when is_port(Port),is_record(RRDSpec,rrd_file) ->
    rrd_create:do_create(Port,RRDSpec).

%% %% @spec dump(Port::port(),File::string(),ToFile::string()) -> Result
%% %% Port    = port()
%% %% File    = string()
%% %% ToFile  = string()
%% %% Result  = WHAT
%% %% @doc Dump a RRDTool database to an XML-file
%% %%

%% dump(Port,File,ToFile) when is_port(Port) ->
%%     rrd_dump:dump(Port,File,ToFile}).

%% @spec fetch(Port::port(),File::string(),CF::cf(),Res:://utils/time:duration()) -> Result
%%
%% File       = string()
%% Result     = WHAT
%%
%% @doc Fetch data from an RRDTool database file
%%
%% @equiv fetch(Port,File,CF,Res,latest,latest)
%%

fetch(Port,File,CF,Res) when is_port(Port) ->
    rrd_fetch:do_fetch(Port,File,CF,Res,latest,latest).

%% @spec fetch(Port::port(),File::string(),CF::cf(),Res:://utils/time:duration(),Start,Stop) -> Result
%%
%% Start      = //utils/time:datetime() | latest
%% Stop       = //utils/time:datetime() | latest
%% Result     = WHAT
%%
%% @doc Fetch data from an RRDTool database file
%%

fetch(Port,File,CF,Res,Start,Stop) when is_port(Port) ->
    rrd_fetch:do_fetch(Port,File,CF,Res,Start,Stop).

%% @spec graph(Port::port(),Pars::rrd_graph()) -> Result
%% Result = {ok, nothing} | {error, Reason}
%%
%% @doc Create a graph.
%%
%% <p> For a more detailed description of the parameters see the
%% RRDTool <a href="http://......">documentation</a>.</p>
 
graph(Port,Pars) when is_port(Port) ->
    rrd_graph:do_graph(Port,Pars).

%% @spec info(Port::port(),File::string()) -> Result
%% Result    = WHAT
%%
%% @doc Get information about a RRDTool database
%%
%% @equiv info(Port,File,all)

info(Port,File) when is_port(Port) ->
    rrd_info:do_info(Port,File,all).

%% @spec info(Port::port(),File::string(),Type) -> Result
%% Type      = all | last | dss | rras
%% Result    = WHAT
%%
%% @doc Get information about a RRDTool database.
%%
%% <p>When Type=all, returns all information about the database.</p>
%% <p>When Type=last, returns information about the last time the database was updated.</p>
%% <p>When Type=dss, returns information about the datasources in the database.</p>
%% <p>When Type=rra, returns information about the roundrobin archives in the database.</p>

info(Port,File,Type) when is_port(Port), Type==all; Type==last; Type==dss; Type==rras ->
    rrd_info:do_info(Port,File,Type).

%% @spec last(Port::port(),File::string()) -> Result
%% Result   = WHAT
%%
%% @doc Get the timestamp for the last update of a database.
%%

last(Port,File) when is_port(Port) ->
    rrd_last:do_last(Port,File).

%% @spec restore(Port::port(),FromFile::string(),ToFile::string()) -> Result
%% Result   = WHAT
%%
%% @doc Restore the contents of an RRD from an XML dump
%%
%% @equiv restore(Port,[],FromFile,ToFile)

restore(Port,FromFile,ToFile) when is_port(Port) ->
    restore(Port,[],FromFile,ToFile).

%% @spec restore(Port::port(),Opts,FromFile::string(),ToFile::string()) -> Result
%% Opts     = [Opt]
%% Opt      = range_check
%% Result   = WHAT
%%
%% @doc Restore the contents of an RRD from an XML dump
%%

restore(Port,Opts,FromFile,ToFile) when is_port(Port) ->
    rrd_restore:do_restore(Port,Opts,FromFile,ToFile).

%% %% @spec resize(Port::port(),File::string(),RRA::integer(),OP,Rows::integer()) -> Result
%% %% OP       = grow | shrink
%% %%
%% %% @doc Add or delete rows in an RRDTool database.
%% %%

%% resize(Port,File,RRA,OP,Rows) ->
%%     rrd_resize:do_resize(File,RRA,OP,Rows}).

%% tune(Port,File,Pars) ->
%%     rrd_tune:do_tune(File,Pars}).

%% @spec update(Port::port(),File::string(),Values) -> Result
%% ValueSets = [ValueSet]
%% ValueSet  = {n,[Value::number()]} | {TS:://utils/time:datetime(),[Value::number()]}

%% Result   = ok | {error, Reason}
%% Reason = string()
%%
%% @doc Updates an RRD.
%%
%% @equiv update(Port,File,[],Values)

update(Port,File,Values) when is_port(Port), is_list(Values) ->
    update(Port,File,[],Values).

%% @spec update(Port::port(),File::string(),Template,ValueSets) -> Result
%% Template  = [atom()]
%% ValueSets = [ValueSet]
%% ValueSet  = {n,[Value]} | {TS,[Value]}
%% Result    = {ok,[TS:://utils/time:datetime()]} | {error, Reason}
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

%% @spec xport(Port::port(),Pars::rrd_export()) -> Result
%%
%% @doc Get data from RRDTool database.

xport(Port,Pars) when is_port(Port) ->
    rrd_xport:do_xport(Port,Pars).

%% @private
do_cmd(Port,CMD) when is_port(Port) ->
    case catch port_command(Port,CMD) of
	{'EXIT',Port,Reason} ->
  	    {error,Reason};
  	true ->
	    get_resp(Port)
    end.

get_resp(Port) ->
    get_resp(Port,[]).

get_resp(Port,Acc) ->
    receive
	{Port,{data,Res}} ->
	    case done(Res) of
		true ->
		    parse_resp(Acc++Res);
		false  ->
		    get_resp(Port,Acc++Res)
	    end
    end.

done("OK"++_More) ->
    true;
done("ERROR:"++_More) ->
    true;
done(S) ->
    case string:str(S,"\nOK") of
	0 -> false;
	N -> true
    end.

parse_resp("ERROR: "++More) ->
    Res=get_error(More),
    {error,Res};
parse_resp("-1\nERROR: "++More) ->
    Res=get_error(More),
    {error,Res};
parse_resp("OK"++_More) ->
    {ok,nothing};
parse_resp(Resp) ->
    Res=string:left(Resp,string:str(Resp,"\nOK")-1),
    {ok,Res}.

get_error(Res) ->
    case string:str(Res,"\nOK") of
	0 -> 
	    string:left(Res,length(Res)-1);
	N ->
	    string:left(Res,N-1)
    end.
