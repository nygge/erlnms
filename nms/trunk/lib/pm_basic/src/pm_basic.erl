%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Public API for pm_basic.
%%% @end 
%%% Created :  4 Aug 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_basic).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%% @headerfile "pm_store.hrl"
-include("pm_store.hrl").

%% API
%%--------------------------------------------------------------------
%% External exports
-export([
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

-export([start/0,stop/0,
	 create/3,
	 fetch/5,
	 fetch/7,
	 update/4]).


%%====================================================================
%% API
%%====================================================================

%% @spec start() -> ok
%% @doc Start the application.
%% @end
start() ->
    application:start(?MODULE).

%% @spec stop() -> ok
%% @doc Stop the application.
%% @end
stop() ->
    application:stop(?MODULE).

%% @spec get_counter_def(MOI::fdn(),MOC::atom(),Counter::atom()) -> pm_counter()
%% @doc Get the definition of a counter.
%% @end
get_counter_def(MOI,MOC,Cnt) ->
    pm_config:get_counter_def(MOI,MOC,Cnt).

%% @spec get_counters(MOI::mo_instance(),MOC,CType) -> [counter()]
%% @doc Get counters defined for a specific MO.
%% @end
get_counters(MOI,MOC,CType) when CType==primary;CType==derived;
				 CType==all ->
    pm_config:get_counters(MOI,MOC,CType).

%% @spec get_db_backend_cb(Backend::atom()) -> atom()
%% @doc Get the callback module for the database backend Backend.
%% @end
get_db_backend_cb(Backend) ->
    pm_config:get_db_backend_cb(Backend).

%% @spec get_db_backend(MOI,MOC) -> atom()
%% @doc Get the database backend that handles {MOI,MOC}.
%% @end
get_db_backend(MOI,MOC) ->
    pm_config:get_db_backend(MOI,MOC).

%% Db_Backend
%% @spec new_db_backend(DB_Backend::db_backend()) -> ok
%% @doc Add a new database backend.
%% @end
new_db_backend(DB_Backend) when is_record(DB_Backend,pm_db_backend) ->
    pm_config:new_db_backend(DB_Backend).

%% @spec delete_db_backend(Key) -> ok
%% @doc Delete the definition of database backend Key.
%% @end
delete_db_backend(Key) -> 
    pm_config:delete_db_backend(Key).

%% @spec get_db_backend(Key) -> db_backend()
%% @doc Get the definition of database backend Key.
%% @end
get_db_backend(Key) ->
    pm_config:get_db_backend(Key).

%% @spec get_all_db_backends() -> [db_backend()]
%% @doc Get the definition of all database backends.
%% @end
get_all_db_backends() ->
    pm_config:get_all_db_backends().

%% Store_Inst
%% @spec new_store_inst(StoreInst::pm_store_inst()) -> ok
%% @doc Add a new store instance.
%% @end
new_store_inst(Dur) when is_record(Dur,pm_store_inst) ->
    pm_config:new_store_inst(Dur).

%% @spec delete_store_inst(Key::atom()) -> ok
%% @doc Delete the definition of store instance Key.
%% @end
delete_store_inst(Key) ->
    pm_config:delete_store_inst(Key).

%% @spec get_store_inst(Key) -> pm_store_inst()
%% @doc Get the definition of store instance Key.
%% @end
get_store_inst(Key) ->
    pm_config:get_store_inst(Key).

%% @spec get_all_store_insts() -> [pm_store_inst()]
%% @doc Get the definition of all store instances.
%% @end
get_all_store_insts() ->
    pm_config:get_all_store_insts().

%% Store_Type
%% @spec new_store_type(StoreType::pm_store_type()) -> ok
%% @doc Add a new store type.
%% @end
new_store_type(StoreType) when is_record(StoreType,pm_store_type) ->
    pm_config:new_store_type(StoreType).

%% @spec delete_store_type(Key::atom()) -> ok
%% @doc Delete the definition of store type Key.
%% @end
delete_store_type(Key) ->
    pm_config:delete_store_type(Key).

%% @spec get_store_type(Key) -> pm_store_type()
%% @doc Get the definition of store type key.
%% @end
get_store_type(Key) ->
    pm_config:get_store_type(Key).

%% @spec get_all_store_types() -> [pm_store_type()]
%% @doc Get the definition of all store types.
%% @end
get_all_store_types() ->
    pm_config:get_all_store_types().

%% Archive
%% @spec new_archive(Archive::pm_archive()) -> ok
%% @doc Add a new archive.
%% @end
new_archive(Dur) when is_record(Dur,pm_archive) ->
    pm_config:new_archive(Dur).

%% @spec delete_archive(Key::atom()) -> ok
%% @doc Delete the definition of archive Key
%% @end
delete_archive(Key) ->
    pm_config:delete_archive(Key).

%% @spec get_archive(Key) -> pm_archive()
%% @doc Get the definition of archive Key.
%% @end
get_archive(Key) ->
    pm_config:get_archive(Key).

%% @spec get_all_archives() -> [pm_archive()]
%% @doc Get the definition of all archives.
%% @end
get_all_archives() ->
    pm_config:get_all_archives().

%% Aggregate
%% @spec new_aggregate(Aggregate::pm_aggregate()) -> ok
%% @doc Add a new aggregate.
%% @end
new_aggregate(Dur) when is_record(Dur,pm_aggregate) ->
    pm_config:new_aggregate(Dur).

%% @spec delete_aggregate(Key) -> ok
%% @doc Delete the definition of aggregate Key.
%% @end
delete_aggregate(Key) ->
    pm_config:delete_aggregate(Key).

%% @spec get_aggregate(Key) -> pm_aggregate()
%% @doc Get the definition of aggregate Key.
%% @end
get_aggregate(Key) ->
    pm_config:get_aggregate(Key).

%% @spec get_all_aggregates() -> [pm_aggregate()]
%% @doc Get the definition of all aggregates.
%% @end
get_all_aggregates() ->
    pm_config:get_all_aggregates().

%% Mo_Type
%% @spec new_mo_type(MO_type::pm_mo_type()) -> ok
%% @doc Add a new mo type.
%% @end
new_mo_type(MO_type) when is_record(MO_type,pm_mo_type) ->
    pm_config:new_mo_type(MO_type).

%% @spec delete_mo_type(Key::atom()) -> ok
%% @doc Delete the definition of mo type Key.
%% @end
delete_mo_type(Key) ->
    pm_config:delete_mo_type(Key).

%% @spec get_mo_type(Key::atom()) -> pm_mo_type()
%% @doc Get the definition of mo type Key.
%% @end
get_mo_type(Key) ->
    pm_config:get_mo_type(Key).

%% @spec get_all_mo_types() -> [pm_mo_type()]
%% @doc Get the definition of all mo types.
%% @end
get_all_mo_types() ->
    pm_config:get_all_mo_types().

%% Counter
%% @spec new_counter(Counter::pm_counter()) -> ok
%% @doc Add a new counter.
%% @end
new_counter(Counter) when is_record(Counter,pm_counter) ->
    pm_config:new_counter(Counter).

%% @spec delete_counter(Key::atom()) -> ok
%% @doc Delete the definition of counter Key.
%% @end
delete_counter(Key) ->
    pm_config:delete_counter(Key).

%% @spec get_counter(Key) -> pm_counter()
%% @doc Get the definition of counter Key.
%% @end
get_counter(Key) ->
    pm_config:get_counter(Key).

%% @spec get_all_counters() -> [pm_counter()]
%% @doc Get the definition of all counters.
%% @end
get_all_counters() ->
    pm_config:get_all_counters().

%% Der_Counter
%% @spec new_der_counter(DerCounter::pm_der_counter()) -> ok
%% @doc Add a new derived counter.
%% @end
new_der_counter(DerCounter) ->
    pm_config:new_der_counter(DerCounter).

%% @spec delete_der_counter(Key::atom()) -> pm_der_counter()
%% @doc Delete the definition of derived counter Key.
%% @end
delete_der_counter(Key) ->
    pm_config:delete_der_counter(Key).

%% @spec get_der_counter(Key) -> pm_der_counter()
%% @doc Get the definition of derived counter Key.
%% @end
get_der_counter(Key) ->
    pm_config:get_der_counter(Key).

%% @spec get_all_der_counters() -> [pm_der_counter()]
%% @doc Get the definition of all derived counters.
%% @end
get_all_der_counters() ->
    pm_config:get_all_der_counters().

%% Duration
%% @spec new_duration(Dur::pm_duration()) -> ok
%% @doc Add a new duration.
%% @end
new_duration(Dur) when is_record(Dur,pm_duration) ->
    pm_config:new_duration(Dur).

%% @spec delete_duration(Key::atom()) -> ok
%% @doc Delete the definition of duration Key.
%% @end
delete_duration(Key) ->
    pm_config:delete_duration(Key).

%% @spec get_duration(Key) -> pm_duration()
%% @doc Get the definition of duration Key.
%% @end
get_duration(Key) ->
    pm_config:get_duration(Key).

%% @spec get_all_durations() -> [pm_duration()]
%% @doc Get the definition of all durations.
%% @end
get_all_durations() ->
    pm_config:get_all_durations().

%% Events
%% @spec new_event(Event::pm_event()) -> ok
%% @doc Add a new event.
%% @end
new_event(Event) when is_record(Event,pm_event) ->
    pm_config:new_event(Event).

%% @spec delete_event(Key) -> ok
%% @doc Delete the definition of event Key.
%% @end
delete_event(Key) ->
    pm_config:delete_event(Key).

%% @spec get_event(Key) -> pm_event()
%% @doc Get the definition of event Key.
%% @end
get_event(Key) ->
    pm_config:get_event(Key).

%% @spec get_events(MOI,MOC) -> pm_event()
%% @doc Get the definition of all events for a specific measurement object, {MOI,MOC}.
%% @end
get_events(MOI,MOC) ->
    pm_config:get_events(MOI,MOC).

%% @spec get_all_events() -> pm_event()
%% @doc Get the definition of all events.
%% @end
get_all_events() ->
    pm_config:get_all(pm_event).

%% @spec get_store_def(Name) -> What
%% @doc Get the definition of store Name.
%% @end
get_store_def(Name) ->
    pm_config:get_store_def(Name).


%% @spec create(MOI,Store_type,BackEnd) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% Store_type  = atom()
%% BackEnd     = atom()
%% Result      = {atomic,Reply}
%% Reply       = WHAT
%% @doc Create a measurement store.
%% @end

create(MOI,StoreType,BackEnd) when is_list(MOI),
				   is_atom(StoreType),
				   is_atom(BackEnd) ->
    pm_store:create(MOI,StoreType,BackEnd).

%% fetch(MOI,MOC,Res) ->
%%     {found,Counters}=pm_config:get_counters(MOI,MOC,all),
%%     fetch(MOI,MOC,Counters,Res).

%% @spec fetch(MOI,MOC,Counters,CF,Res::duration()) -> Result
%% MOI         = [RDN]
%% RDN         = {Type::atom(),Id::atom()}
%% MOC         = atom()
%% Counters    = [Counter::atom()]
%% CF          = atom
%% Result      = WHAT
%% @doc Fetch the latest row of data from the performance data store.
%% @end

fetch(MOI,MOC,Counters,CF,Res) ->
    pm_store:fetch(MOI,MOC,Counters,CF,Res).

%% @spec fetch(MOI,MOC,Counters,CF,Res,From,Stop) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Counters    = [Counter]
%% Counter     = atom()
%% CF          = atom
%% Res         = Duration
%% From        = Rows|Start
%% Rows        = integer()
%% Start       = datetime()
%% Stop        = datetime()
%% Result      = WHAT
%% @doc Fetch data from the performance data store.
%% <p>If From is an integer() fetch will return From rows of data ending
%% at time Stop.</p>
%% <p>If From is a datetime() fetch will return rows of data starting at time Start and ending at time Stop.</p>
%% @end

fetch(MOI,MOC,Counters,CF,Res,Rows,Stop) when is_integer(Rows),Rows>0 ->
    pm_store:fetch(MOI,MOC,Counters,CF,Res,Rows,Stop);

%% @spec fetch(MOI,MOC,Counters,CF,Res,Start,Stop) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Counters    = [Counter]
%% Counter     = atom()
%% CF          = atom
%% Res         = Duration
%% Start       = datetime()
%% Stop        = datetime()
%% Result      = WHAT
%% @doc Fetch data from the performance data store.
%% @end

fetch(MOI,MOC,Counters,CF,Res,Start,Stop) when is_tuple(Start) ->
    pm_store:fetch(MOI,MOC,Counters,CF,Res,Start,Stop).

%% @spec update(MOI,MOC,Time,Data) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Time        = datetime()
%% Result      = {ok,[TimeStamp]}
%% TimeStamp   = integer()
%% @doc Insert data in the performance data store.
%% @end

update(MOI,MOC,Time,Data) ->
    pm_store:update(MOI,MOC,Time,Data).
%%====================================================================
%% Internal functions
%%====================================================================
