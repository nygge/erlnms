%%------------------------------------------------------------
%% key   = MOI
%% db    = Database backend

-record(pm_db_backend,{key,db}).

%%------------------------------------------------------------
%% name          = atom
%% value         = duration_spec
%% duration_spec = {type,int}
%% type          = sec|min|hour|day|week|month|year

-record(pm_duration,{name,value}).

%%------------------------------------------------------------
%% id       = [RDN], MOI
%% RDN      = {atom,term}
%% events   = [Duration]
%% Duration = {unit,int}
%% unit     = sec|min|hour|day|week|month|year

-record(pm_event,{id,events}).

