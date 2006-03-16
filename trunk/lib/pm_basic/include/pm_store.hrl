%%------------------------------------------------------------
%% name       = {moi,mo_type}
%% store_type = atom, pm_mes_type,
%% backend    = atom

-record(pm_store_inst,{name,store_type,backend}).

%%------------------------------------------------------------
%% name          = atom, record name
%% mo_type       = atom, pm_mo_type
%% archive       = atom, pm_archive
%% step          = duration_spec
%% duration_spec = {type,int}
%% type          = sec|min|hour|day|week|month|year

-record(pm_store_type,{name,mo_type,archive,step}).

%%------------------------------------------------------------
%% name      = atom, record name
%% agregates = [atom], names of aggregates

-record(pm_archive,{name, aggregates}).

%%------------------------------------------------------------
%% name          = atom
%% cf            = min|max|last|average
%% xff           = float
%% resolution    = duration_spec|atom
%% duration      = duration_spec|atom
%% duration_spec = {type,int}
%% type          = sec|min|hour|day|week|month|year

-record(pm_aggregate,{name, cf, xff, resolution, duration}).

%%------------------------------------------------------------
%% name          = atom, record name
%% counters      = [atom], names of counters 
%% der_counters  = [atom], names of derived counters 

-record(pm_mo_type,{name, counters, der_counters}).

%%------------------------------------------------------------
%% name          = atom
%% type          = gauge|counter|derive|absolute
%% hb            = duration_spec
%% duration_spec = {type,int}
%% type          = sec|min|hour|day|week|month|year
%% min, max      = none|atom|float|int

-record(pm_counter, {name, type, hb, min="U", max="U"}).

%%------------------------------------------------------------
%% name = atom
%% expr = RPN 
%% deps = [atom], names of counters

-record(pm_der_counter,{name,expr,deps}).

