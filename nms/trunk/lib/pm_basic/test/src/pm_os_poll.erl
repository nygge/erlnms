%%%-------------------------------------------------------------------
%%% File    : pm_os_poll.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 14 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_os_poll).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 create_aggregates/0,
	 create_archives/0,
	 create_counters/0,
	 create_durations/0,
%	 create_file/0,
	 create_mo_types/0,
	 create_store_inst/0,
	 create_store_type/0,
	 graph/0,
	 fetch/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("pm_store.hrl").
-include("pm_rec.hrl").

-record(state, {int=5000,file="/tmp/RRD_avg.rrd"}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_durations() ->
    pm_basic:new_duration(#pm_duration{name=sec5,value={sec,5}}),
    pm_basic:new_duration(#pm_duration{name=sec30,value={sec,30}}),
    pm_basic:new_duration(#pm_duration{name=min5,value={min,5}}),
    pm_basic:new_duration(#pm_duration{name=min30,value={min,30}}),
    pm_basic:new_duration(#pm_duration{name=hour1,value={hour,1}}),
    pm_basic:new_duration(#pm_duration{name=hour6,value={hour,6}}),
    pm_basic:new_duration(#pm_duration{name=day1,value={day,1}}),
    pm_basic:new_duration(#pm_duration{name=week2,value={week,2}}).

create_counters() ->
    pm_basic:new_counter(#pm_counter{name={load_avg,avg1},type='GAUGE',
				     hb=sec30,min=0,max=1024}),
    pm_basic:new_counter(#pm_counter{name={load_avg,avg5},type='GAUGE',
				     hb=sec30,min=0,max=1024}),
    pm_basic:new_counter(#pm_counter{name={load_avg,avg15},type='GAUGE',
				     hb=sec30,min=0,max=1024}).

create_mo_types() ->
    pm_basic:new_mo_type(#pm_mo_type{name=load_avg,
				     counters=[avg1,avg5,avg15],
				     der_counters=[]}).

create_aggregates() ->
    pm_basic:new_aggregate(#pm_aggregate{name=last_sec5_min5,
					 cf='LAST',xff=0.5,
					 resolution=sec5,duration=min5}),
    pm_basic:new_aggregate(#pm_aggregate{name=last_min5_hour6,
					 cf='LAST',xff=0.5,
					 resolution=min5,duration=hour6}),
    pm_basic:new_aggregate(#pm_aggregate{name=last_min30_day1,
					 cf='LAST',xff=0.5,
					 resolution=min30,duration=day1}),
    pm_basic:new_aggregate(#pm_aggregate{name=last_hour1_week2,
					 cf='LAST',xff=0.5,
					 resolution=hour1,duration=week2}).

create_archives() ->
    pm_basic:new_archive(#pm_archive{name=arch1,
				     aggregates=[last_sec5_min5,
						 last_min5_hour6,
						 last_min30_day1,
						 last_hour1_week2]}).
create_store_type() ->
    pm_basic:new_store_type(#pm_store_type{name=load_avg_std,
					   mo_type=load_avg,
					   archive=arch1,
					   step=sec5}).

create_store_inst() ->
    pm_basic:new_store_inst(#pm_store_inst{name={[{ne,"godot"},{cpu,1}],laptop},
					   store_type=load_avg_std,
					   backend=rrdtool}).

graph()->
    gen_server:call(?MODULE,graph).

fetch() ->
    gen_server:call(?MODULE,fetch).

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
    State=#state{},
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

handle_call(fetch, _From, State) ->
    Reply=pm_basic:fetch([{ne,"godot"},{cpu,1}],load_avg,[avg1,avg5,avg15],
			 'LAST',{sec,30},10,calendar:universal_time()),
    {reply, Reply, State};


handle_call(graph, _From, State) ->
    %%     Now=erlang:universaltime(),
    %%     Start=time:time_calc(Now,{min,-5}),
    %%     DEFs=[#rrd_def{vname=avg1,
    %% 		   rrd="/tmp/RRD_avg.rrd",
    %% 		   ds_name=avg1,
    %% 		   cf='LAST'}, 
    %% 	  #rrd_def{vname=avg5,
    %% 		   rrd="/tmp/RRD_avg.rrd",
    %% 		   ds_name=avg5,
    %% 		   cf='LAST'}, 
    %% 	  #rrd_def{vname=avg15,
    %% 		   rrd="/tmp/RRD_avg.rrd",
    %% 		   ds_name=avg15,
    %% 		   cf='LAST'}],
    %%     Lines=[#rrd_line{width=1,vname=avg1,color=16#FF0000,legend="avg1"},
    %% 	   #rrd_line{width=1,vname=avg5,color=16#00FF00,legend="avg5"},
    %% 	   #rrd_line{width=1,vname=avg15,color=16#0000FF,legend="avg15"}],
    %%     G=#rrd_graph{file="/tmp/RRD_avg.gif",
    %% 		 start=Start,
    %% 		 stop=Now,
    %% 		 step={sec,5},
    %% 		 defs=DEFs,
    %% 		 %%cdefs,
    %% 		 %%vdefs,
    %% 		 graph=Lines
    %% 		 %%options
    %% 		},
    %%     Reply=rrd_lib:graph(State#state.port,G),
    Reply=ok,
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
    TS=calendar:universal_time(),
    Rec=#pm_rec{moi=[{ne,"godot"},{cpu,1}],
		moc=load_avg,
		time=TS,
		data=[A1,A5,A15]},
    pm_raw_data:send(Rec),
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
