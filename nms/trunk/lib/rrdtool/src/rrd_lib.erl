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
%%%
%%% @type heartbeat() = //utils/time:duration().
%%% Max number of seconds between two updates.
%%%
%%%
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

%% @headerfile "rrdtool.hrl"
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
%%    Cmd="/usr/pkg/bin/rrdtool -",
    Cmd="/usr/bin/rrdtool -",
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
	_N -> true
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
