%%%-------------------------------------------------------------------
%%% File    : rrdtool.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2003-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%% Created : 26 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrdtool).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-define(SERVER,?MODULE).
%%--------------------------------------------------------------------
%% External exports
-export([start/0,stop/0,
	 start_link/0,start_link/1,
	 create/1,
%%	 dump/1,
	 fetch/3,
	 fetch/5,
	 graph/2,
	 info/1,info/2,info/3,
	 last/1,
	 restore/2,restore/3,
%%	 rrdresize/4,
%%	 tune/2,
	 update/2,update/3,update/4,
	 xport/1,xport_async/1,
	 subscribe/1
	]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-include("rrdtool.hrl").

-record(state, {locking,waiting,working,work}).
-record(waiting, {id,cmd}).
%-record(cmd, {id,c}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------

%% @spec start_link() -> {ok,Pid} | something
%% @doc Start the rrdtool server.
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% @spec start_link(Name) -> {ok,Pid} | something
%% Name  = atom()
%% @doc Start the rrdtool server.
%% The server will be registered with the name {local,Name}.

start_link(Name) when is_atom(Name) ->
    gen_server:start_link({local,Name}, ?MODULE, [], []);

%% @spec start_link({Type,Name}) -> {ok,Pid} | something
%% Type  = local|global
%% Name  = atom()
%% @doc Start the rrdtool server.
%% The server will be registered with the name {Type,Name}.
start_link({Type,Name}) when is_atom(Type),is_atom(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @spec start() -> something
%% @doc Start the rrdtool application.
%% @equiv application:start(rrdtool)

start() ->
    application:start(rrdtool).

%% @spec stop() -> something
%% @doc Stop the rrdtool application.
%% @equiv application:stop(rrdtool)

stop() ->
    application:stop(rrdtool).

%% @spec create(FileSpec::rrd_lib:rrd_file()) -> Result
%%
%% Result = {ok,nothing} | {error,Reason}
%%
%% @doc Create a RRDTool database file.
%%
%% <p> For a more detailed description of the parameters see the
%% RRDTool <a href="http://......">documentation</a>.</p>
%% @see rrd_lib:create/2

create(RRDSpec) when is_record(RRDSpec,rrd_file) ->
    gen_server:call(?MODULE,{cmd,create,RRDSpec}).

%% %% @spec dump(File::string(),ToFile::string()) -> Result
%% %% Result  = WHAT
%% %% @doc Dump a RRDTool database to an XML-file
%% %%

% dump(File,ToFile) ->
%     gen_server:call(?MODULE,{dump,File,ToFile}).

%% @spec fetch(File::string(),CF::rrd_lib:cf(),Res::rrd_lib:duration()) -> Result
%%
%% Result     = WHAT
%%
%% @doc Fetch data from an RRDTool database file
%%
%% @equiv fetch(Port,File,CF,Res,latest,latest)
%%

fetch(File,CF,Res) ->
    gen_server:call(?MODULE,{cmd,fetch,{File,CF,Res,latest,latest}},infinity).

%% @spec fetch(File::string(),CF::rrd_lib:cf(),Res::rrd_lib:duration(),Start,Stop) -> Result
%%
%% Start      = rrd_lib:datetime() | latest
%% Stop       = rrd_lib:datetime() | latest
%% Result     = {DSs,[ROW]}
%% DSs        = [atom()]
%% ROW        = {rrd_lib:datetime(),[Value]}
%% Value      = float() | unknown
%%
%% @doc Fetch data from an RRDTool database file
%% @see rrd_lib:fetch/6

fetch(File,CF,Res,Start,Stop) ->
    gen_server:call(?MODULE,{cmd,fetch,{File,CF,Res,Start,Stop}}).

%% @spec graph(File::string(),Pars::rrd_lib:rrd_graph()) -> Result
%%
%% Result = {ok, nothing} | {error, Reason}
%%
%% @doc Create a graph.
%%
%% <p> For a more detailed description of the parameters see the
%% RRDTool <a href="http://......">documentation</a>.</p>
%% @see rrd_lib:graph/3

graph(File,Pars) ->
    gen_server:call(?MODULE,{cmd,graph,{File,Pars}}).

%% @spec info(File) -> Result
%% File      = string()
%% Result    = WHAT
%%
%% @doc Get information about a RRDTool database
%%
%% @equiv info(File,all)

info(File) ->
    gen_server:call(?MODULE,{cmd,info,{File,all}}).

%% @spec info(File,Timeout) -> Result
%% File      = string()
%% Timeout   = integer()
%% Result    = {File,Version,Step,Last,DSSs,RRAs}

%%
%% @doc Get information about a RRDTool database
%%
%% @equiv info(File,all,Timeout)

info(File,Timeout) when Timeout==infinity; is_integer(Timeout) ->
    info(File,all,Timeout);

%% @spec info(File,Type) -> Result
%% File      = string()
%% Type      = all | last | dss | rra
%% Result    = WHAT
%%
%% @doc Get information about a RRDTool database.
%%
%% @equiv info(File,Type,infinity)
%%
%% <p>When Type=all, returns all information about the database.</p>
%% <p>When Type=last, returns information about the last time the database was updated.</p>
%% <p>When Type=dss, returns information about the datasources in the database.</p>
%% <p>When Type=rra, returns information about the roundrobin archives in the database.</p>
%% @see rrd_lib:info/3
info(File,Type) when Type==all; Type==last; Type==dss; Type==rra ->
    gen_server:call(?MODULE,{cmd,info,{File,Type}}).

%% @spec info(File,Type,Timeout) -> Result
%% File      = string()
%% Type      = all | last | dss | rra
%% Timeout   = integer()
%% Result    = All | Last | DSSs | RRAs
%% All       = {FileName,Version,Step,Last,DSSs,RRAs}
%% FileName  = {filename,string()}
%% Version   = {version,string}
%% Step      = {step,Duration}
%% Last      = {lastupdate,Datetime}
%% DSSs      = {dss,[DSS]}
%% DSS       = {DSName,[DS]}
%% DS        = {DSType,HB,MIN,MAX,LAST,Val,UNKOWNSECS}
%% RRAs      = {rra,[RRA]}
%% RRA       = {CF,DUR,RES,XFF}
%% CF        = {cf,CFun}
%% CFun      = 'LAST' | 'MAX' | 'MIN' | 'AVG'
%% DUR       = {Unit,integer()}
%% RES       = {Unit,integer()}
%% Unit      = sec | min | hour | day | week | month | year
%% XFF       = {xff,float}
%%
%% @doc Get information about a RRDTool database.
%%
%% <p>When Type=all, returns all information about the database.</p>
%% <p>When Type=last, returns information about the last time the database was updated.</p>
%% <p>When Type=dss, returns information about the datasources in the database.</p>
%% <p>When Type=rra, returns information about the roundrobin archives in the database.</p>

info(File,Type,Timeout) when Type==all; Type==last; Type==dss; Type==rra,
			     Timeout==infinity; is_integer(Timeout)->
    gen_server:call(?MODULE,{cmd,info,{File,Type}},Timeout).

%% @spec last(File) -> Result
%% File     = string()
%% Result   = datetime()
%%
%% @doc Get the timestamp for the last update of a database.
%% @see rrd_lib:last/2

last(File) ->
    gen_server:call(?MODULE,{cmd,last,{File}}).

%% @spec restore(FromFile,ToFile) -> Result
%% FromFile = string()
%% ToFile   = string()
%% Result   = WHAT
%%
%% @doc Restore the contents of an RRD from an XML dump
%%
%% @equiv restore([],FromFile,ToFile)

restore(FromFile,ToFile) ->
    restore([],FromFile,ToFile).

%% @spec restore(Opts,FromFile,ToFile) -> Result
%% Opts     = [Opt]
%% Opt      = range_check
%% FromFile = string()
%% ToFile   = string()
%% Result   = WHAT
%%
%% @doc Restore the contents of an RRD from an XML dump
%% @see rrd_lib:restore/4

restore(Opts,FromFile,ToFile) ->
    gen_server:call(?MODULE,{cmd,restore,{Opts,FromFile,ToFile}}).

% rrdcgi() ->
%     gen_server:call(?MODULE,{rrdcgi,}).

%% %% @spec resize(File,RRA,OP,Rows) -> Result
%% %% File     = string()
%% %% RRA      = atom()
%% %% OP       = grow | shrink
%% %% Rows     = integer()
%% %%
%% %% @doc Add or delete rows in an RRDTool database.
%% %%
% rrdresize(File,RRA,OP,Rows) ->
%     gen_server:call(?MODULE,{rrdresize,File,RRA,OP,Rows}).

% tune(File,Pars) ->
%     gen_server:call(?MODULE,{tune,File,Pars}).

%% @spec update(File,Values) -> Result
%% File     = string()
%% ValueSets = [ValueSet]
%% ValueSet  = {n,[Value]} | {TS,[Value]}
%% Result   = ok | {error, Reason}
%% Reason = string()
%%
%% @doc Updates an RRD.
%%
%% @equiv update(File,[],Values)

update(File,Values) when is_list(Values) ->
    update(File,[],Values).

% update(File,Values,Timeout) when is_list(Values); 
% 				 Timeout==infinity; is_integer(Timeout)->
%     update(File,[],Values,Timeout);

%% @spec update(File,Template,ValueSets) -> Result
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
%% @equiv update(File,[],Values,infinity)

update(File,Template,Values) when is_list(File),
				  is_list(Template), 
				  is_list(Values) ->
    update(File,Template,Values,infinity).

%% @spec update(File,Template,ValueSets,Timeout) -> Result
%% File      = string()
%% Template  = [atom()]
%% ValueSets = [ValueSet]
%% ValueSet  = {n,[Value]} | {TS,[Value]}
%% Result    = {ok,TSs} | {error, Reason}
%% TSs       = [TS]
%% TS        = integer()
%% Timeout   = integer() | infinity
%% Reason    = string()
%%
%% @doc Updates an RRD.
%% @see rrd_lib:update/4

update(File,Template,Values,Timeout) when is_list(File),
					  is_list(Template), 
					  is_list(Values),
					  Timeout==infinity; is_integer(Timeout)
					  ->
    gen_server:call(?MODULE,{cmd,update,{File,Template,Values}},Timeout).

%% @spec xport(Pars::rrd_lib:rrd_export()) -> Result
%% @doc Get data from RRDTool database.
%% @see rrd_lib:xport/2

xport(Pars) ->
    gen_server:call(?MODULE,{cmd,xport,Pars}).

%% @spec xport_async(Pars::rrd_lib:rrd_export()) -> Result
%% @doc Asynchronous get data from RRDTool database.
%% @see rrd_lib:xport/2

xport_async(Pars) ->
    gen_server:cast(?MODULE,{cmd,xport_async,{self(),Pars}}).

%% @spec subscribe(Filter) -> Result
%% Filter     = match_spec
%% @doc Subscribe to events from rrdtool.
%% Filter is a match specification, see ?????? for details.

subscribe(Filter) ->
   subscription_event_h:subscribe(rrdtool_event,self(),Filter).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
%% @private
init([]) ->
    process_flag(trap_exit, true),
%%    {ok,No_workers}=application:get_env(no_rrdtool_workers),
    No_workers=3,
    Workers=start_workers(No_workers),
    Locking=ets:new(locking,[set,protected,{keypos,2}]),
    {ok,#state{locking=Locking,
      	       waiting=Workers,
	       working=[],
	       work=queue:new()}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% @private
handle_call({cmd,CMD,Pars}, From, State) ->
    {Lock,Resources}=find_lock(CMD,Pars),
    rrdtool_lock:lock(From,Lock,Resources),
    ets:insert(State#state.locking,#waiting{id=From,cmd={CMD,Pars}}),
    {noreply,State}.

%% handle_call(Request, From, State) ->
%%     Reply = ok,
%%     {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% @private

handle_cast(_Msg, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% @private

handle_info({locked,Id}, State) -> 
    [#waiting{id=Id,cmd=CMD}]=ets:lookup(State#state.locking,Id),
    ets:delete(State#state.locking,Id),
    NewState=new_work(Id,CMD,State),
    {noreply,NewState};

handle_info({done,Cmd,Worker,Id,Res}, State) ->
    rrdtool_lock:release(Id),
    do_reply(Cmd,Id,Res),
    Working=State#state.working,
    NS1=State#state{working=lists:keydelete(Worker,1,Working)},
    NS2=get_more_work(Worker,NS1),
    {noreply,NS2};

handle_info({'EXIT',Pid,_Reason},State) ->
    Working=State#state.working,
    case lists:keysearch(Pid,1,Working) of
	{value,{Pid,Id}} ->
	    rrdtool_lock:release(Id),
	    {ok,NewWorker}=start_worker(0),
	    NS1=State#state{working=lists:keydelete(Pid,1,Working)},
	    NS2=get_more_work(NewWorker,NS1),
	    {noreply,NS2};
	false ->
	    {noreply,State}
    end;

handle_info(_Info, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) -> 
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_workers(NoWorkers) ->
    [start_worker(N)||N<- lists:seq(1,NoWorkers)].

start_worker(_Dummy) ->
    {ok,Pid}=rrdtool_worker:start_link(),
    Pid.
	 
find_lock(create,RRDSpec) ->
    {write,[RRDSpec#rrd_file.file]};
find_lock(fetch,{File,_CF,_Res,_Start,_Stop}) ->
    {read,[File]};
find_lock(graph,{_File,Pars}) ->
    {value,DEFs}=lists:keysearch(def,1,Pars),
    Files=[RRD||{_VName,RRD,_DSName,_CF}<-DEFs],
    {read,lists:usort(Files)};
find_lock(info,{File,_Type}) ->
    {read,[File]};
find_lock(last,{File}) ->
    {read,[File]};
find_lock(restore,{_FromFile,ToFile}) ->
    {write,[ToFile]};
find_lock(update,{File,_Template,_Values}) ->
    {write,[File]};
find_lock(xport,{_Flags,DEFs,_CDEFs,_XPORTs}) ->
    Files=[RRD||{_VName,RRD,_DSName,_CF}<-DEFs],
    {read,lists:usort(Files)}.

new_work(Id,CMD,State=#state{locking=_L,waiting=[],working=_W,work=Wqueue})->
    State#state{work=queue:in(Wqueue,{Id,CMD})};
new_work(Id,CMD,State=#state{locking=_L,waiting=[Worker|Ws],working=W,work=_Wqueue}) ->
    feed_worker(Worker,Id,CMD),
    State#state{waiting=Ws,working=[{Worker,Id}|W]}.

get_more_work(Worker,State=#state{locking=_L,waiting=Wt,working=W,work=Wqueue})->
    case queue:len(Wqueue) of
	0 ->
	    State#state{waiting=[Worker|Wt]};
	_N ->
	    {{value,{Id,CMD}},Q2}=queue:out(Wqueue),
	    feed_worker(Worker,Id,CMD),
	    State#state{working=[{Worker,Id}|W],work=Q2}
    end.

feed_worker(Worker,Id,CMD) ->
    Worker!{cmd,Id,CMD}.


do_reply(update_async,_Id,_Res)->
    ok;
do_reply(_Cmd,Id,Res) ->
    gen_server:reply(Id,Res).
