%%%-------------------------------------------------------------------
%%% File    : pm_config.erl
%%% Created : 25 May 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc pm_config handles all configuration data for pm_basic.
%%% @end
%%%-------------------------------------------------------------------
-module(pm_config).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("pm_store.hrl").

-define(SERVER,?MODULE).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,start_link/1,

	 new_db_backend/1,get_db_backend/1,get_db_backend/2,
	 get_all_db_backends/0,delete_db_backend/1,

	 new_store_inst/1,get_store_inst/1,
	 get_all_store_insts/0,delete_store_inst/1,

	 new_store_type/1,get_store_type/1,get_store_def/1,
	 get_all_store_types/0,delete_store_type/1,

	 new_archive/1,get_archive/1,
	 get_all_archives/0,delete_archive/1,

	 new_aggregate/1,get_aggregate/1,
	 get_all_aggregates/0,delete_aggregate/1,

	 new_mo_type/1,get_mo_type/1,
	 get_all_mo_types/0,delete_mo_type/1,

	 new_counter/1,get_counter/1,
	 get_all_counters/0,delete_counter/1,

	 new_der_counter/1,get_der_counter/1,
	 get_all_der_counters/0,delete_der_counter/1,
         get_counters/3,get_counter_def/3,

	 new_duration/1,get_duration/1,
	 get_all_durations/0,delete_duration/1,

	 new_event/1,get_event/1,get_events/2,
	 get_all_events/0,delete_event/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,pid()}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link({local, ?SERVER}).

%% @spec start_link(Name) -> {ok,pid()}
%% @doc Starts the server and register it with name Name.
%% @end
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @spec get_counter_def(MOI,MOC,Cnt) -> counter()
%% @doc Get the definition of a counter.
%% @end
get_counter_def(MOI,MOC,Cnt) ->
    gen_server:call(?SERVER,{get_counter_def,MOI,MOC,Cnt},infinity).

%% @spec get_counters(MOI::mo_instance(),MOC,CType) -> [counter()]
%% @doc Get counters defined for a specific MO.
%% @end
get_counters(MOI,MOC,CType) when CType==primary;CType==derived;
				 CType==all ->
    gen_server:call(?SERVER,{get_counters,MOI,MOC,CType}).

%% @spec get_db_backend_cb(Backend::atom()) -> atom()
%% @doc Get the callback module for the database backend Backend.
%% @end
get_db_backend_cb(Backend) ->
    gen_server:call(?SERVER,{get_db_backend_cb,Backend},infinity).

%% @spec get_db_backend(MOI,MOC) -> atom()
%% @doc Get the database backend that handles {MOI,MOC}.
%% @end
get_db_backend(MOI,MOC) ->
    gen_server:call(?SERVER,{get_db_backend,MOI,MOC},infinity).

%% Db_Backend
%% @spec new_db_backend(DB_Backend::db_backend()) -> ok
%% @doc Add a new database backend.
%% @end
new_db_backend(DB_Backend) when is_record(DB_Backend,pm_db_backend) ->
    gen_server:call(?SERVER,{new,DB_Backend},infinity).

%% @spec delete_db_backend(Key) -> ok
%% @doc Delete the definition of database backend Key.
%% @end
delete_db_backend(Key) -> 
    gen_server:call(?SERVER,{delete,pm_db_backend,Key},infinity).

%% @spec get_db_backend(Key) -> db_backend()
%% @doc Get the definition of database backend Key.
%% @end
get_db_backend(Key) ->
    gen_server:call(?SERVER,{get,pm_db_backend,Key},infinity).

%% @spec get_all_db_backends() -> [db_backend()]
%% @doc Get the definition of all database backends.
%% @end
get_all_db_backends() ->
    gen_server:call(?SERVER,{get_all,pm_db_backend},infinity).

%% Store_Inst
%% @spec new_store_inst(StoreInst::pm_store_inst()) -> ok
%% @doc Add a new store instance.
%% @end
new_store_inst(Dur) when is_record(Dur,pm_store_inst) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

%% @spec delete_store_inst(Key::atom()) -> ok
%% @doc Delete the definition of store instance Key.
%% @end
delete_store_inst(Key) ->
    gen_server:call(?SERVER,{delete,pm_store_inst,Key},infinity).

%% @spec get_store_inst(Key) -> pm_store_inst()
%% @doc Get the definition of store instance Key.
%% @end
get_store_inst(Key) ->
    gen_server:call(?SERVER,{get,pm_store_inst,Key},infinity).

%% @spec get_all_store_insts() -> [pm_store_inst()]
%% @doc Get the definition of all store instances.
%% @end
get_all_store_insts() ->
    gen_server:call(?SERVER,{get_all,pm_store_inst},infinity).

%% Store_Type
%% @spec new_store_type(StoreType::pm_store_type()) -> ok
%% @doc Add a new store type.
%% @end
new_store_type(StoreType) when is_record(StoreType,pm_store_type) ->
    gen_server:call(?SERVER,{new,StoreType},infinity).

%% @spec delete_store_type(Key::atom()) -> ok
%% @doc Delete the definition of store type Key.
%% @end
delete_store_type(Key) ->
    gen_server:call(?SERVER,{delete,pm_store_type,Key},infinity).

%% @spec get_store_type(Key) -> pm_store_type()
%% @doc Get the definition of store type key.
%% @end
get_store_type(Key) ->
    gen_server:call(?SERVER,{get,pm_store_type,Key},infinity).

%% @spec get_all_store_types() -> [pm_store_type()]
%% @doc Get the definition of all store types.
%% @end
get_all_store_types() ->
    gen_server:call(?SERVER,{get_all,pm_store_type},infinity).

%% Archive
%% @spec new_archive(Archive::pm_archive()) -> ok
%% @doc Add a new archive.
%% @end
new_archive(Dur) when is_record(Dur,pm_archive) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

%% @spec delete_archive(Key::atom()) -> ok
%% @doc Delete the definition of archive Key
%% @end
delete_archive(Key) ->
    gen_server:call(?SERVER,{delete,pm_archive,Key},infinity).

%% @spec get_archive(Key) -> pm_archive()
%% @doc Get the definition of archive Key.
%% @end
get_archive(Key) ->
    gen_server:call(?SERVER,{get,pm_archive,Key},infinity).

%% @spec get_all_archives() -> [pm_archive()]
%% @doc Get the definition of all archives.
%% @end
get_all_archives() ->
    gen_server:call(?SERVER,{get_all,pm_archive},infinity).

%% Aggregate
%% @spec new_aggregate(Aggregate::pm_aggregate()) -> ok
%% @doc Add a new aggregate.
%% @end
new_aggregate(Dur) when is_record(Dur,pm_aggregate) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

%% @spec delete_aggregate(Key) -> ok
%% @doc Delete the definition of aggregate Key.
%% @end
delete_aggregate(Key) ->
    gen_server:call(?SERVER,{delete,pm_aggregate,Key},infinity).

%% @spec get_aggregate(Key) -> pm_aggregate()
%% @doc Get the definition of aggregate Key.
%% @end
get_aggregate(Key) ->
    gen_server:call(?SERVER,{get,pm_aggregate,Key},infinity).

%% @spec get_all_aggregates() -> [pm_aggregate()]
%% @doc Get the definition of all aggregates.
%% @end
get_all_aggregates() ->
    gen_server:call(?SERVER,{get_all,pm_aggregate},infinity).

%% Mo_Type
%% @spec new_mo_type(MO_type::pm_mo_type()) -> ok
%% @doc Add a new mo type.
%% @end
new_mo_type(MO_type) when is_record(MO_type,pm_mo_type) ->
    gen_server:call(?SERVER,{new,MO_type},infinity).

%% @spec delete_mo_type(Key::atom()) -> ok
%% @doc Delete the definition of mo type Key.
%% @end
delete_mo_type(Key) ->
    gen_server:call(?SERVER,{delete,pm_mo_type,Key},infinity).

%% @spec get_mo_type(Key::atom()) -> pm_mo_type()
%% @doc Get the definition of mo type Key.
%% @end
get_mo_type(Key) ->
    gen_server:call(?SERVER,{get,pm_mo_type,Key},infinity).

%% @spec get_all_mo_types() -> [pm_mo_type()]
%% @doc Get the definition of all mo types.
%% @end
get_all_mo_types() ->
    gen_server:call(?SERVER,{get_all,pm_mo_type},infinity).

%% Counter
%% @spec new_counter(Counter::pm_counter()) -> ok
%% @doc Add a new counter.
%% @end
new_counter(Counter) when is_record(Counter,pm_counter) ->
    gen_server:call(?SERVER,{new,Counter},infinity).

%% @spec delete_counter(Key::atom()) -> ok
%% @doc Delete the definition of counter Key.
%% @end
delete_counter(Key) ->
    gen_server:call(?SERVER,{delete,pm_counter,Key},infinity).

%% @spec get_counter(Key) -> pm_counter()
%% @doc Get the definition of counter Key.
%% @end
get_counter(Key) ->
    gen_server:call(?SERVER,{get,pm_counter,Key},infinity).

%% @spec get_all_counters() -> [pm_counter()]
%% @doc Get the definition of all counters.
%% @end
get_all_counters() ->
    gen_server:call(?SERVER,{get_all,pm_counter},infinity).

%% Der_Counter
%% @spec new_der_counter(DerCounter::pm_der_counter()) -> ok
%% @doc Add a new derived counter.
%% @end
new_der_counter(DerCounter) ->
    gen_server:call(?SERVER,{new,DerCounter},infinity).

%% @spec delete_der_counter(Key::atom()) -> pm_der_counter()
%% @doc Delete the definition of derived counter Key.
%% @end
delete_der_counter(Key) ->
    gen_server:call(?SERVER,{delete,pm_der_counter,Key},infinity).

%% @spec get_der_counter(Key) -> pm_der_counter()
%% @doc Get the definition of derived counter Key.
%% @end
get_der_counter(Key) ->
    gen_server:call(?SERVER,{get,pm_der_counter,Key},infinity).

%% @spec get_all_der_counters() -> [pm_der_counter()]
%% @doc Get the definition of all derived counters.
%% @end
get_all_der_counters() ->
    gen_server:call(?SERVER,{get_all,pm_der_counter},infinity).

%% Duration
%% @spec new_duration(Dur::pm_duration()) -> ok
%% @doc Add a new duration.
%% @end
new_duration(Dur) when is_record(Dur,pm_duration) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

%% @spec delete_duration(Key::atom()) -> ok
%% @doc Delete the definition of duration Key.
%% @end
delete_duration(Key) ->
    gen_server:call(?SERVER,{delete,pm_duration,Key},infinity).

%% @spec get_duration(Key) -> pm_duration()
%% @doc Get the definition of duration Key.
%% @end
get_duration(Key) ->
    gen_server:call(?SERVER,{get,pm_duration,Key},infinity).

%% @spec get_all_durations() -> [pm_duration()]
%% @doc Get the definition of all durations.
%% @end
get_all_durations() ->
    gen_server:call(?SERVER,{get_all,pm_duration},infinity).

%% Events
%% @spec new_event(Event::pm_event()) -> ok
%% @doc Add a new event.
%% @end
new_event(Event) when is_record(Event,pm_event) ->
    gen_server:call(?SERVER,{new,Event},infinity).

%% @spec delete_event(Key) -> ok
%% @doc Delete the definition of event Key.
%% @end
delete_event(Key) ->
    gen_server:call(?SERVER,{delete,pm_event,Key},infinity).

%% @spec get_event(Key) -> pm_event()
%% @doc Get the definition of event Key.
%% @end
get_event(Key) ->
    gen_server:call(?SERVER,{get,pm_event,Key},infinity).

%% @spec get_events(MOI,MOC) -> pm_event()
%% @doc Get the definition of all events for a specific measurement object, {MOI,MOC}.
%% @end
get_events(MOI,MOC) ->
    gen_server:call(?SERVER,{get_events,MOI,MOC},infinity).

%% @spec get_all_events() -> pm_event()
%% @doc Get the definition of all events.
%% @end
get_all_events() ->
    gen_server:call(?SERVER,{get_all,pm_event},infinity).

%% @spec get_store_def(Name) -> What
%% @doc Get the definition of store Name.
%% @end
get_store_def(Name) ->
    gen_server:call(?SERVER,{get_store_def,Name},infinity).

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
%% @private
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
%% @private
%%--------------------------------------------------------------------
handle_call({new,Record}, _From, State) ->
    Reply=insert_tab(Record),
    {reply,Reply,State};

handle_call({delete,Tab,Key}, _From, State) ->
    Reply=delete(Tab,Key),
    {reply,Reply,State};

handle_call({get,Tab,Key}, _From, State) ->
    Reply=read_tab(Tab,Key),
    {reply,Reply,State};

handle_call({get_all,Tab}, _From, State) ->
    Reply=get_all(Tab),
    {reply,Reply,State};

%%---------------

handle_call({get_db_backend_cb,Name}, _From, State) ->
    Reply=case read_tab(pm_db_backen,Name) of
	      #pm_db_backend{name=Name,module=Mod} ->
		  {reply,{found,Mod},State};
	      [] ->
		  {reply,not_found,State}
	  end,
    {reply,Reply,State};

handle_call({get_db_backend,MOI,MOC}, _From, State) ->
    Reply=case read_tab(pm_store_inst,{MOI,MOC}) of
	      DB when is_record(DB,pm_store_inst)->
		  {reply,{found,DB#pm_store_inst.backend},State};
	      [] ->
		  {reply,not_found,State}
	  end,
    {reply,Reply,State};

handle_call({get_counter_def,MOI,MOC,Cnt},_From,State) ->
    CD=case read_tab(pm_store_inst,{MOI,MOC}) of
	   Rec when is_record(Rec,pm_store_inst) ->
	       ST = read_tab(pm_store_type,Rec#pm_store_inst.store_type),
	       MOC_rec = read_tab(pm_mo_type,ST#pm_store_type.mo_type),
	       case lists:member(Cnt,MOC_rec#pm_mo_type.counters) of
		   true ->
		       {counter,Cnt};
		   false ->
		       case lists:member(Cnt,MOC_rec#pm_mo_type.der_counters) of
			   true ->
			       DC=read_tab(pm_der_counter,Cnt),
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
	Rec when is_record(Rec,pm_store_inst) ->
	    ST = read_tab(pm_store_type,Rec#pm_store_inst.store_type),
	    MOT = read_tab(pm_mo_type,ST#pm_store_type.mo_type),
	    Counters =case CType of
			  primary ->
			      MOT#pm_mo_type.counters;
			  derived ->
			      MOT#pm_mo_type.der_counters;
			  all ->
			      MOT#pm_mo_type.counters++MOT#pm_mo_type.der_counters
		      end,
	    {reply,{found,Counters},State};
	[] ->
	    {reply,not_found,State}
    end;

handle_call({get_events,MOI,MOC}, _From, State) ->
    case read_tab(pm_event,MOI) of
	Es when is_record(Es,pm_event) ->
	    case read_tab(pm_store_inst,{MOI,MOC}) of
		Rec when is_record(Rec,pm_store_inst) ->
		    case read_tab(pm_store_type,Rec#pm_store_inst.store_type) of
			ST when is_record(ST,pm_store_type) ->
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
    #pm_store_type{name=Name,
		   mo_type=MOC,
		   archive=Arch,
		   step=Step}=read_tab(pm_store_type,Name),
    #pm_mo_type{name=MOC,
		counters=Counters,
		der_counters=_DC}=read_tab(pm_mo_type,MOC),
    Cs=lists:map(fun (C) ->
			 Cnt=read_tab(pm_counter,{MOC,C}),
			 HB=read_tab(pm_duration,Cnt#pm_counter.hb),
			 Cnt#pm_counter{hb=HB}
		 end, Counters),
    #pm_archive{name=Arch,
		aggregates=AGRS}=read_tab(pm_archive,Arch),
    As=lists:map(fun(A) ->
			 R=read_tab(pm_aggregate,A),
			 Dur=read_tab(pm_duration,R#pm_aggregate.duration),
			 Res=read_tab(pm_duration,R#pm_aggregate.resolution),
			 R#pm_aggregate{resolution=Res,duration=Dur}
		 end, AGRS),
    Result=#pm_store_type{name=Name,
			  mo_type=#pm_mo_type{name=MOC,
					      counters=Cs},
			  archive=#pm_archive{name=Arch,
					      aggregates=As},
			  step=read_tab(pm_duration,Step)},
    {reply,Result,State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% @private
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

insert_tab(Record) when is_record(Record,pm_db_backend) ->
    write(Record);

insert_tab(Record) when is_record(Record,pm_store_inst) ->
    case exists(pm_store_type,Record#pm_store_inst.store_type) and
	exists(pm_db_backend,Record#pm_store_inst.backend) of
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
    end;

insert_tab(Record) when is_record(Record,pm_duration) ->
    write(Record);

insert_tab(Record) when is_record(Record,pm_event) ->
    write(Record).

delete(pm_db_backend,Key) ->
    delete_row(pm_db_backend,Key);

delete(pm_store_inst,Key) ->
    delete_row(pm_store_inst,Key);

delete(pm_store_type,Key) ->
    delete_row(pm_store_type,Key);

delete(pm_archive,Key) ->
    delete_row(pm_archive,Key);

delete(pm_aggregate,Key) ->
    delete_row(pm_aggregate,Key);

delete(pm_mo_type,Key) ->
    delete_row(pm_mo_type,Key);

delete(pm_counter,Key) ->
    delete_row(pm_counter,Key);

delete(pm_der_counter,Key) ->
    delete_row(pm_der_counter,Key);

delete(pm_duration,Key) ->
    delete_row(pm_duration,Key);

delete(pm_event,Key) ->
    delete_row(pm_event,Key).


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
	Rec when is_tuple(Rec)->
	    true;
	[] ->
	    false
    end.

read_tab(Table,Key) ->
    case mnesia:dirty_read({Table,Key}) of
	[] ->
	    [];
	[Res] ->
	    Res
    end.
    
write(Record) ->
    F=fun() ->
	      mnesia:write(Record)
      end,
    mnesia:transaction(F).

get_all(Tab) ->
    [hd(mnesia:dirty_read(Tab,Key)) || Key <- mnesia:dirty_all_keys(Tab)].

delete_row(Tab,Key) ->
    F=fun() ->
	      mnesia:delete({Tab,Key})
      end,
    mnesia:transaction(F).
    
