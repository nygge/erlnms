%% @type cf() = min|max|last|average.
%% Consolidation function.

%%------------------------------------------------------------
%% @type pm_db_backend() = #pm_db_backend{name=atom(), module=atom()}.

-record(pm_db_backend,{name,module}).

%%------------------------------------------------------------
%% @type pm_store_inst() = #pm_store_inst{name={Moi::atom(),MO_Type::atom()},store_type=PM_mes_type::atom(),backend=atom()}.
%% MOI is FDN.
-record(pm_store_inst,{name,store_type,backend}).

%%------------------------------------------------------------
%% @type pm_store_type() = #pm_store_type{name=atom(),mo_type=atom(),archive=atom(),step=duration()}.

-record(pm_store_type,{name,mo_type,archive,step}).

%%------------------------------------------------------------
%% @type pm_archive() = #pm_archive{name=atom(), aggregates=[Aggregate::atom()]}.

-record(pm_archive,{name, aggregates}).

%%------------------------------------------------------------
%% @type pm_aggregate() = #pm_aggregate{name=atom(), cf=cf(), xff=float(), resolution=duration()|atom(), duration=duration()|atom()}.

-record(pm_aggregate,{name, cf, xff, resolution, duration}).

%%------------------------------------------------------------
%% @type pm_mo_type() = #pm_mo_type{name=atom(), counters=[Counter::atom()], der_counters=[DCounter::atom()]}.

-record(pm_mo_type,{name, counters, der_counters}).

%%------------------------------------------------------------
%% @type pm_counter() = #pm_counter{name={MOtype::atom(),Name::atom()}, type=gauge|counter|derive|absolute, hb=duration(), min=none|atom()|float()|int(), max=none|atom()|float()|int()}.

-record(pm_counter, {name, type, hb, min="U", max="U"}).

%%------------------------------------------------------------
%% @type pm_der_counter() = #pm_der_counter{name=atom(),expr=string(),deps=[Counter::atom()]}.

-record(pm_der_counter,{name,expr,deps}).

%%------------------------------------------------------------
%% @type pm_duration() = #pm_duration{name=atom(),value=duration()}.

-record(pm_duration,{name,value}).

%%------------------------------------------------------------
%% @type pm_event() = #pm_event{id=fdn(),events=[duration()]}.

-record(pm_event,{id,events}).

