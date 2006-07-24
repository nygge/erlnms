%%%-------------------------------------------------------------------
%%% File    : pm_rrd_access.erl
%%% Created :  5 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc PM database backend based on RRDtool.
%%% @end
%%%-------------------------------------------------------------------
-module(pm_rrd_access).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%%-include("pm_rec.hrl").
-include("pm_store.hrl").
-include("pm_rrdtool.hrl").
-include("rrdtool.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([create/2,
	 fetch/7,
	 update/4
	]).

%%====================================================================
%% External functions
%%====================================================================

%% @spec create(MOI,Store_type) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% Store_type  = atom()
%% Result      = {atomic,Reply}
%% Reply       = WHAT
%% @doc Create a measurement store.

create(MOI,Store) when is_list(MOI) ->
    MO_TYPE=(Store#pm_store_type.mo_type)#pm_mo_type.name,
    MOT=Store#pm_store_type.mo_type,
    Arch=Store#pm_store_type.archive,
    File=mk_file_name(MOI,MO_TYPE),
    Cmd=#rrd_file{file=File,
		  step=(Store#pm_store_type.step)#pm_duration.value,
		  dss=mkDSs(MOT#pm_mo_type.counters),
		  rras=mkRRAs(Arch#pm_archive.aggregates)},
    Res={ok,nothing}=rrdtool:create(Cmd),
    {atomic,_Reply}=pm_rrd_config:new_store_inst(#pm_rrd_inst{name={MOI,MO_TYPE},
							      file=File}),
    Res.

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

fetch(MOI,MOC,Counters,CF,Res,Start,Stop) when is_tuple(Start) ->
    {PCs,DCs}=order_counters(MOI,MOC,Counters),
    DEFs=mk_defs(PCs,CF),
    CDEFs=mk_cdefs(DCs),
    XPORTs=[{Name,Name}||Name <- Counters],
    {DEFs,CDEFs},
    rrdtool:xport({[{step,Res},{start,Start},{'end',Stop}],DEFs,CDEFs,XPORTs}).

%% @spec update(MOI,MOC,Time,Data) -> Result
%% MOI         = [RDN]
%% RDN         = {Type,Id}
%% Type        = atom()
%% Id          = atom()
%% MOC         = atom()
%% Time        = datetime()
%% Result      = {ok,[TimeStamp]}
%% TimeStamp   = integer()

update(MOI,MOC,Time,Data) ->
    {found,File}=pm_rrd_config:id_to_file(MOI,MOC),
    {found,Counters}=pm_rrd_config:get_counters(MOI,MOC,primary),
    TS1=time:datetime_to_epoch(Time),
    {ok,[TS1]}=rrdtool:update(File,Counters,[{TS1,Data}]).

%%====================================================================
%% Internal functions
%%====================================================================

mk_file_name(MOI,Meas_type) ->
    {ok,Root}={ok,"/home/anders/src/data/pm/"}, %%%application:get_env(root_dir),
    Path=[Root]++lists:foldr(fun ({C,I},Acc) ->
				   [C,I]++Acc
			   end, 
			   [], MOI),
    CList=[to_string(P)||P<-Path],
    Path1=filename:join(CList),
    File=filename:join(Path1,to_string(Meas_type)++".rrd"),
    filelib:ensure_dir(File),
    File.

mkDSs(Counters) ->
    [#rrd_ds{name=N,type=T,hb=HB#pm_duration.value,min=Min,max=Max} ||
	#pm_counter{name={_,N},type=T,hb=HB,min=Min,max=Max} <- Counters].

mkRRAs(Aggregates) ->
    [#rrd_rra{cf=CF,xff=XFF,
	      interval=Int#pm_duration.value,
	      duration=Dur#pm_duration.value} ||
	#pm_aggregate{name=_,cf=CF,xff=XFF,resolution=Int,duration=Dur} 
	    <- Aggregates].

to_string(X) when is_atom(X) ->
    atom_to_list(X);
to_string(X) when is_integer(X) ->
    integer_to_list(X);
to_string(X) when is_list(X) ->
    X.

order_counters(MOI,MOC,Counters) ->
    {PCs,DCs}=lists:foldl(fun (Cnt,Acc) ->
			   add_counter(MOI,MOC,[Cnt],Acc)
		   end,{[],[]},Counters),
    {lists:reverse(PCs),lists:reverse(DCs)}.

% add_counter(MOI,MOC,Counters) ->
%     add_counter(MOI,MOC,Counters,{[],[]}).

add_counter(MOI,MOC,Counters,Acc) ->
    lists:foldl(fun (C,Acc1) ->
			add_counter1(MOI,MOC,
				     pm_rrd_config:get_counter_def(MOI,MOC,C),
				     Acc1)
		end,
		Acc,Counters).

add_counter1(_MOI,_MOC,{counter,Cnt,File},{PCs,DCs}=Acc) ->
    case lists:keymember(Cnt,1,PCs) of
	true ->
	    Acc;
	false ->
	    {[{Cnt,File}|PCs],DCs}
    end;
add_counter1(MOI,MOC,{d_counter,Cnt,Expr,Deps,_File},{_PCs,DCs}=Acc) ->
    case lists:keymember(Cnt,1,DCs) of
	true ->
	    Acc;
	false ->
	    {PCs1,DCs1}=add_counter(MOI,MOC,Deps,Acc),
	    {PCs1,[{Cnt,Expr}|DCs1]}
    end.


mk_defs(PCs,CF) ->
    [{Name,File,Name,CF} || {Name,File} <- PCs].
mk_cdefs(DCs) ->
    DCs.

