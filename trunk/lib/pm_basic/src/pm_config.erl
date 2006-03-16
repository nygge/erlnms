%%%-------------------------------------------------------------------
%%% File    : pm_config.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 25 May 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_config).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("pm_config.hrl").
-include("pm_store.hrl").

-define(SERVER,?MODULE).


%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,start_link/1,
	 get_db_backend/2,
	 get_duration/1,
	 get_events/2
	]).

% from pm_rrd_config
-export([exists/2,
	 get/2,
         get_counters/3,get_counter_def/3,
         get_events/2,
         insert/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link({local, ?SERVER}).
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

get(Tab,Name) ->
    gen_server:call(?SERVER,{get,Tab,Name},infinity).

get_counter_def(MOI,MOC,Cnt) ->
    gen_server:call(?SERVER,{get_counter_def,MOI,MOC,Cnt},infinity).

get_counters(MOI,MOC,CType) when CType==primary;CType==derived;
				 CType==all ->
    gen_server:call(?SERVER,{get_counters,MOI,MOC,CType}).

get_db_backend(MOI,MOC) ->
    gen_server:call(?SERVER,{get_db_backend,MOI,MOC},infinity).

get_duration(Dur) ->
    gen_server:call(?SERVER,{get_duration,Dur},infinity).

get_events(MOI,MOC) ->
    gen_server:call(?SERVER,{get_events,MOI,MOC},infinity).

insert(Record) ->
    gen_server:call(?SERVER,{insert,Record}).

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
init([]) ->
    {ok, #state{}}.

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
handle_call({get_db_backend,MOI,MOC}, _From, State) ->
    Reply=case read_tab(pm_store_inst,{MOI,MOC}) of
	      [DB] ->
		  {reply,{found,DB#pm_store_inst.backend},State};
	      [] ->
		  {reply,not_found,State}
	  end,
    {reply,Reply,State};

handle_call({get_duration,D}, _From, State) when is_atom(D) ->
    [DurRec]=read_tab(pm_duration,D),
    {reply,DurRec#pm_duration.value,State};
handle_call({get_duration,D}, _From, State) when is_tuple(D)->
    {reply,D,State};

handle_call({get,Table,Name}, _From, State) ->
    Reply=read_tab(Table,Name),
    {reply, Reply, State};

handle_call({get_counter_def,MOI,MOC,Cnt},_From,State) ->
    CD=case read_tab(pm_store_inst,{MOI,MOC}) of
	   [Rec] ->
	       [ST] = read_tab(pm_store_type,Rec#pm_store_inst.store_type),
	       [MOC_rec] = read_tab(pm_mo_type,ST#pm_store_type.mo_type),
	       case lists:member(Cnt,MOC_rec#pm_mo_type.counters) of
		   true ->
		       {counter,Cnt};
		   false ->
		       case lists:member(Cnt,MOC_rec#pm_mo_type.der_counters) of
			   true ->
			       [DC]=read_tab(pm_der_counter,Cnt),
			       Expr=DC#pm_der_counter.expr,
			       Deps=DC#pm_der_counter.deps,
			       {d_counter,Cnt,Expr,Deps};
			   false ->
			       exit({counter_does_not_exists,MOI,MOC,Cnt})
		       end
	       end;
	   _Error ->
	       not_found
       end,
    {reply,CD,State};

handle_call({get_counters,MOI,MOC,CType}, _From, State) ->
    case read_tab(pm_store_inst,{MOI,MOC}) of
	[Rec] ->
	    [ST] = read_tab(pm_store_type,Rec#pm_store_inst.store_type),
	    [MOT] = read_tab(pm_mo_type,ST#pm_store_type.mo_type),
	    Counters =case CType of
			  primary ->
			      MOT#pm_mo_type.counters;
			  derived ->
			      MOT#pm_mo_type.der_counters;
			  all ->
			      MOT#pm_mo_type.counters++MOT#pm_mo_type.der_counters
		      end,
	    {reply,{found,Counters},State};
	_Other ->
	    {reply,not_found,State}
    end;

handle_call({get_events,MOI,MOC}, _From, State) ->
    case read_tab(pm_event,MOI) of
	[Es] ->
	    case read_tab(pm_store_inst,{MOI,MOC}) of
		[Rec] ->
		    case read_tab(pm_store_type,Rec#pm_store_inst.store_type) of
			[ST] ->
			    {reply, 
			     {found,Es#pm_event.events,ST#pm_store_type.step}, 
			     State};
			[] ->
			    {reply,not_found,State}
		    end;
		_Other ->
		    {reply, not_found, State}
	    end;
	[] ->
	    {reply, not_found, State}
    end;

handle_call({get_store_def,Name}, _From, State) ->
    STORE_TYPE=read_tab(pm_store_type,Name),
    [#pm_store_type{name=Name,mo_type=MOC,archive=Arch,step=Step}]=STORE_TYPE,
    MO_TYPE=read_tab(pm_mo_type,MOC),
    [#pm_mo_type{name=MOC,counters=Counters,der_counters=_DC}]=MO_TYPE,
    Cs=lists:map(fun (C) ->
			 [#pm_counter{name={MOC,C},
				      type=T,
				      hb=HB,
				      min=MIN,
				      max=MAX}]=read_tab(pm_counter,{MOC,C}),
			 HB1=pm_config:get_duration(HB),
			 {C,T,HB1,MIN,MAX}
		 end, Counters),
    ARec=read_tab(pm_archive,Arch),
    [#pm_archive{name=Arch,aggregates=AGRS}]=ARec,
    As=lists:map(fun(A) ->
			 [R]=read_tab(pm_aggregate,A),
			 Dur=pm_config:get_duration(R#pm_aggregate.duration),
			 Res=pm_config:get_duration(R#pm_aggregate.resolution),
			 {R#pm_aggregate.cf,R#pm_aggregate.xff,Res,Dur}
		 end, AGRS),
    {reply,{{step,pm_config:get_duration(Step)},Cs,As},State};

handle_call({insert,Record}, _From, State) ->
    Reply=insert_tab(Record),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

read_tab(Table,Key) ->
    mnesia:dirty_read({Table,Key}).
    
write(Record) ->
    F=fun() ->
	      mnesia:write(Record)
      end,
    mnesia:transaction(F).

insert_tab(Record) when is_record(Record,pm_store_inst) ->
    case exists(pm_store_type,Record#pm_store_inst.store_type) of
	true ->
	    write(Record);
	false ->
	    {error,parts_dont_exists}
    end;

insert_tab(Record) when is_record(Record,pm_store_type) ->
    case exists(pm_mo_type,Record#pm_store_type.mo_type) and
	exists(pm_archive,Record#pm_store_type.archive) of
	true ->
	    write(Record);
	false ->
	    {error,parts_dont_exists}
    end;

insert_tab(Record) when is_record(Record,pm_archive) ->
    case exists(pm_aggregate,Record#pm_archive.aggregates) of
	true ->
	    write(Record);
	false ->
	    {error,parts_dont_exists}
    end;

insert_tab(Record) when is_record(Record,pm_aggregate) ->
    case exists(pm_duration,Record#pm_aggregate.resolution) and 
	exists(pm_duration,Record#pm_aggregate.duration) of
	true ->
	    write(Record);
	false ->
	    {error,parts_dont_exists}
    end;

insert_tab(Record) when is_record(Record,pm_mo_type) ->
    case exists(pm_counter,Record#pm_mo_type.counters) and 
	exists(pm_der_counter,Record#pm_mo_type.der_counters) of
	true ->
	    write(Record);
	false ->
	    {error,parts_dont_exists}
    end;

insert_tab(Record) when is_record(Record,pm_counter) ->
    write(Record);

insert_tab(Record) when is_record(Record,pm_der_counter)->
    Deps=Record#pm_der_counter.deps,
    case deps_exists(Deps) of
	true ->
	    write(Record);
	false ->
	    {error, deps_not_exists}
    end.

deps_exists(Names) when is_list(Names) ->
    lists:foldr(fun (N,Acc) ->
			(exists(pm_counter,N) or exists(pm_der_counter,N)) and Acc
		end,
		true, Names).

exists(Tab,Names) when is_list(Names) ->
    lists:foldr(fun (N,Acc) ->
			exists(Tab,N) and Acc
		end,
		true, Names);
exists(Tab,Name) when is_atom(Name) ->
    case read_tab(Tab,Name) of
	[_Rec] ->
	    true;
	[] ->
	    false
    end.

