%%%-------------------------------------------------------------------
%%% File    : os_poll.erl
%%% Author  : Anders Nygren <anders@telteq.com.mx>
%%% Description : 
%%%
%%% Created : 14 Mar 2006 by Anders Nygren <anders@telteq.com.mx>
%%%-------------------------------------------------------------------
-module(os_poll).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 create_file/0,
	 graph/0,
	 xport/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("rrdtool.hrl").

-record(state, {port,int=5000,file="/tmp/RRD_avg.rrd"}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_file() ->
    SampleInt={sec,5},
    File="/tmp/RRD_avg.rrd",
    DSs=[#rrd_ds{name="avg1",type='GAUGE',hb={sec,30},min=0,max=1024},
	 #rrd_ds{name="avg5",type='GAUGE',hb={sec,30},min=0,max=1024},
	 #rrd_ds{name="avg15",type='GAUGE',hb={sec,30},min=0,max=1024}],
    RRAs=[#rrd_rra{cf='LAST',xff=0.5,interval={sec,5},duration={min,5}},
	  #rrd_rra{cf='LAST',xff=0.5,interval={min,5},duration={hour,6}},
	  #rrd_rra{cf='LAST',xff=0.5,interval={min,30},duration={day,1}},
	  #rrd_rra{cf='LAST',xff=0.5,interval={hour,1},duration={week,2}}],
    Spec=#rrd_file{file=File,step=SampleInt,dss=DSs,rras=RRAs},
    {ok,Port}=rrd_lib:open(),
    rrd_lib:create(Port,Spec),
    rrd_lib:close(Port).

graph()->
    gen_server:call(?MODULE,graph).

xport() ->
    gen_server:call(?MODULE,export).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok,Port}=rrd_lib:open(),
    State=#state{port=Port},
    timer:send_interval(State#state.int,self(),poll),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% handle_call(graph, _From, State) ->
%%     File="/tmp/RRD_avg.png",
%%     Flags={flags,all_flags()},
%%     DEF={def,[{vname1,rrd1,ds_name1,cf1},
%% 	      {vname2,rrd2,ds_name2,cf2}]},
%%     CDEF={cdef,[{vname1,expr1},{vname2,expr2}]},
%%     PRINT={print,[{vname,cf,format}]},
%%     COMMENT={comment,[comment]},
%%     HRULE={hrule,[color,legend]},
%%     VRULE={vrule,[color,legend]},
%%     LINE={line,[{1,vname,color,legend},{2,vname,color,legend}]},
%%     AREA={area,[]},
%%     STACK={stack,[]},
%%     Pars=[Flags,DEF,CDEF,PRINT,COMMENT,HRULE,VRULE],
%%     rrd_lib:graph(State#state.port,File,Pars),
%%     Reply = ok,
%%     {reply, Reply, State};

handle_call(export, _From, State) ->
    DEFs=[#rrd_def{vname=avg1,
		   rrd=State#state.file,
		   ds_name=avg1,
		   cf='LAST'},
	  #rrd_def{vname=avg5,
		   rrd=State#state.file,
		   ds_name=avg5,
		   cf='LAST'},
	  #rrd_def{vname=avg15,
		   rrd=State#state.file,
		   ds_name=avg15,
		   cf='LAST'}],
    XPORTs=[#rrd_xport{vname=avg1,
		       legend=avg1},
	    #rrd_xport{vname=avg5,
		       legend=avg5},
	    #rrd_xport{vname=avg15,
		       legend=avg15}],
    Now=erlang:universaltime(),
    Start=time:time_calc(Now,{min,-5}),
    XPORT=#rrd_export{start=Start,   % Start time
		      stop=Now,      % End time
		      rows=120,       % Max number of rows
		      step={sec,5},  % Data point step
		      defs=DEFs,     % [rrd_def]
		      cdefs=[],      % [rrd_cdef]
		      xports=XPORTs  % [rrd_xport]
		     },

    %% -record(rrd_cdef,{vname,     % Variable name
    %% 		  rpn        % RPN expression
    %% 		 }).

    %% -record(rrd_vdef,{vname,     % Variable name
    %% 		  rpn        % RPN expression
    %% 		 }).

    Reply=rrd_lib:xport(State#state.port,XPORT),
    {reply, Reply, State};

handle_call(graph, _From, State) ->
    Now=erlang:universaltime(),
    Start=time:time_calc(Now,{min,-5}),
    DEFs=[#rrd_def{vname=avg1,
		   rrd="/tmp/RRD_avg.rrd",
		   ds_name=avg1,
		   cf='LAST'}, 
	  #rrd_def{vname=avg5,
		   rrd="/tmp/RRD_avg.rrd",
		   ds_name=avg5,
		   cf='LAST'}, 
	  #rrd_def{vname=avg15,
		   rrd="/tmp/RRD_avg.rrd",
		   ds_name=avg15,
		   cf='LAST'}],
    Lines=[#rrd_line{width=1,vname=avg1,color=16#FF0000},
	   #rrd_line{width=1,vname=avg5,color=16#00FF00},
	   #rrd_line{width=1,vname=avg15,color=16#0000FF}],
    G=#rrd_graph{file="/tmp/RRD_avg.gif",
		 start=Start,
		 stop=Now,
		 step={sec,5},
		 defs=DEFs,
		 %%cdefs,
		 %%vdefs,
		 graph=Lines
		 %%options
		},
    Reply=rrd_lib:graph(State#state.port,G),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(poll, State) ->
    A1=cpu_sup:avg1(),
    A5=cpu_sup:avg5(),
    A15=cpu_sup:avg15(),
    rrd_lib:update(State#state.port,State#state.file,[{n,[A1,A5,A15]}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
