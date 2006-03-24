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
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link({local, ?SERVER}).
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @spec get_counter_def(MOI,MOC,Cnt) -> Result
%% @doc Get the definition of a counter.
get_counter_def(MOI,MOC,Cnt) ->
    gen_server:call(?SERVER,{get_counter_def,MOI,MOC,Cnt},infinity).

get_counters(MOI,MOC,CType) when CType==primary;CType==derived;
				 CType==all ->
    gen_server:call(?SERVER,{get_counters,MOI,MOC,CType}).

get_db_backend_cb(Backend) ->
    gen_server:call(?SERVER,{get_db_backend_cb,Backend},infinity).

get_db_backend(MOI,MOC) ->
    gen_server:call(?SERVER,{get_db_backend,MOI,MOC},infinity).


%% Db_Backend
new_db_backend(Dur) when is_record(Dur,pm_db_backend) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_db_backend(Key) ->
    gen_server:call(?SERVER,{delete,pm_db_backend,Key},infinity).

get_db_backend(Key) ->
    gen_server:call(?SERVER,{get,pm_db_backend,Key},infinity).

get_all_db_backends() ->
    gen_server:call(?SERVER,{get_all,pm_db_backend},infinity).

%% Store_Inst
new_store_inst(Dur) when is_record(Dur,pm_store_inst) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_store_inst(Key) ->
    gen_server:call(?SERVER,{delete,pm_store_inst,Key},infinity).

get_store_inst(Key) ->
    gen_server:call(?SERVER,{get,pm_store_inst,Key},infinity).

get_all_store_insts() ->
    gen_server:call(?SERVER,{get_all,pm_store_inst},infinity).

%% Store_Type
new_store_type(Dur) when is_record(Dur,pm_store_type) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_store_type(Key) ->
    gen_server:call(?SERVER,{delete,pm_store_type,Key},infinity).

get_store_type(Key) ->
    gen_server:call(?SERVER,{get,pm_store_type,Key},infinity).

get_all_store_types() ->
    gen_server:call(?SERVER,{get_all,pm_store_type},infinity).

%% Archive
new_archive(Dur) when is_record(Dur,pm_archive) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_archive(Key) ->
    gen_server:call(?SERVER,{delete,pm_archive,Key},infinity).

get_archive(Key) ->
    gen_server:call(?SERVER,{get,pm_archive,Key},infinity).

get_all_archives() ->
    gen_server:call(?SERVER,{get_all,pm_archive},infinity).

%% Aggregate
new_aggregate(Dur) when is_record(Dur,pm_aggregate) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_aggregate(Key) ->
    gen_server:call(?SERVER,{delete,pm_aggregate,Key},infinity).

get_aggregate(Key) ->
    gen_server:call(?SERVER,{get,pm_aggregate,Key},infinity).

get_all_aggregates() ->
    gen_server:call(?SERVER,{get_all,pm_aggregate},infinity).

%% Mo_Type
new_mo_type(Dur) when is_record(Dur,pm_mo_type) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_mo_type(Key) ->
    gen_server:call(?SERVER,{delete,pm_mo_type,Key},infinity).

get_mo_type(Key) ->
    gen_server:call(?SERVER,{get,pm_mo_type,Key},infinity).

get_all_mo_types() ->
    gen_server:call(?SERVER,{get_all,pm_mo_type},infinity).

%% Counter
new_counter(Dur) when is_record(Dur,pm_counter) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_counter(Key) ->
    gen_server:call(?SERVER,{delete,pm_counter,Key},infinity).

get_counter(Key) ->
    gen_server:call(?SERVER,{get,pm_counter,Key},infinity).

get_all_counters() ->
    gen_server:call(?SERVER,{get_all,pm_counter},infinity).

%% Der_Counter
new_der_counter(Dur) when is_record(Dur,pm_der_counter) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_der_counter(Key) ->
    gen_server:call(?SERVER,{delete,pm_der_counter,Key},infinity).

get_der_counter(Key) ->
    gen_server:call(?SERVER,{get,pm_der_counter,Key},infinity).

get_all_der_counters() ->
    gen_server:call(?SERVER,{get_all,pm_der_counter},infinity).

%% Duration
new_duration(Dur) when is_record(Dur,pm_duration) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_duration(Key) ->
    gen_server:call(?SERVER,{delete,pm_duration,Key},infinity).

get_duration(Key) ->
    gen_server:call(?SERVER,{get,pm_duration,Key},infinity).

get_all_durations() ->
    gen_server:call(?SERVER,{get_all,pm_duration},infinity).

%% Events
new_event(Dur) when is_record(Dur,pm_event) ->
    gen_server:call(?SERVER,{new,Dur},infinity).

delete_event(Key) ->
    gen_server:call(?SERVER,{delete,pm_event,Key},infinity).

get_event(Key) ->
    gen_server:call(?SERVER,{get,pm_event,Key},infinity).

get_events(MOI,MOC) ->
    gen_server:call(?SERVER,{get_events,MOI,MOC},infinity).

get_all_events() ->
    gen_server:call(?SERVER,{get_all,pm_event},infinity).

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
    
