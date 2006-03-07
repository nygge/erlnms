%%%-------------------------------------------------------------------
%%% File    : threshold_conf.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  6 Feb 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(threshold_conf).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("threshold.hrl").
%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,
	 check_threshold/6,
	 get_cmds/4
	 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER,?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_threshold(SysT,MOI,MOC,Cnt,Val,Step) ->
    gen_server:call(?SERVER,{check_threshold,SysT,MOI,MOC,Cnt,Val,Step},infinity).

get_cmds(SysT,MOI,MOC,Int) ->
    gen_server:call(?SERVER,{get_cmds,SysT,MOI,MOC,Int},infinity).

%%-------------------------------------------------------------------
%% 

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
handle_call({check_threshold,SysT,MOI,MOC,Cnt,Val,Step}, From, State) ->
    Reply=check_threshold1(SysT,MOI,MOC,Cnt,Val,Step),
    {reply, Reply, State};

handle_call({get_cmds,SysT,MOI,MOC,Res}, From, State) ->
    Reply = get_cmds1(SysT,MOI,MOC,Res),
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
check_threshold1(SysT,MOI,MOC,Cnt,Val,Step) ->
    Prev=get_prev_th(MOI,MOC,Cnt,Step),
    Pat=mk_th_pat(SysT,MOI,MOC,Cnt,Val,Step,Prev),
    case mnesia:dirty_select(th_threshold,Pat) of
	[] ->
	    not_found;
	Ms when is_list(Ms) ->
	    R=lists:sort(fun deep_cmp/2,Ms),
	    ThDef=hd(R),
	    set_prev_th(MOI,MOC,Cnt,Step,ThDef#th_threshold.dests),
	    ThDef;
	Error ->
	    erlang:fault(MOI,Cnt,Val,Step)
    end.
    
get_prev_th(MOI,MOC,Cnt,Step) ->
    case mnesia:dirty_read(th_prev,{MOI,MOC,Cnt,Step}) of
	[] ->
	    x_none;
	[#th_prev{key=Key,val=Prev}] ->
	    Prev
    end.
set_prev_th(MOI,MOC,Cnt,Step,Val) ->
    mnesia:dirty_write(#th_prev{key={MOI,MOC,Cnt,Step},val=Val}).
				
mk_th_pat(SysT,MOI,MOC,Cnt,Val,Step,Prev) ->
    WCs=mk_wildcard(MOI),
    Guard=mk_th_guard(Val,Prev),
    Result=['$_'],
    [{#th_threshold{key={ST,WC,MOC,Step,Cnt},min='$1',max='$2',dests='$3'},
      Guard,Result}||WC<-WCs,ST<-[SysT,'*']].

mk_th_guard(Val,x_none) ->
[{'andalso',{'=<','$1',Val},
  {'>','$2',Val}}];
mk_th_guard(Val,Prev) ->
    [{'andalso',{'andalso',{'=<','$1',Val},
		 {'>','$2',Val}},
      {'=/=','$3',Prev}}].

get_cmds1(SysT,MOI,MOC,Int) ->
    Pat=mk_cmd_pat(SysT,MOI,MOC,Int),
    case mnesia:dirty_select(th_cmd,Pat) of
	[] ->
	    not_found;
	Ms when is_list(Ms) ->
	    R=lists:sort(fun deep_cmp/2,Ms),
	    hd(R);
	Error ->
	    erlang:fault(MOI,Int)
    end.

mk_cmd_pat(SysT,MOI,MOC,Int) ->
    WCs=mk_wildcard(MOI),
    [{#th_cmd{key={ST,WC,MOC,Int},cmds='_'},[],['$_']}||WC<-WCs,ST<-[SysT,'*']].
    
deep_cmp(X,Y) ->
    deep_cmp(X,Y,fun wc_cmp/2).

deep_cmp(X,Y,Fun) when is_tuple(X),is_tuple(Y), size(X)==size(Y) ->
    deep_cmp(tuple_to_list(X),tuple_to_list(Y),Fun);
deep_cmp(X,Y,Fun) when is_tuple(X),is_tuple(Y) ->
    Fun(size(X),size(Y));
deep_cmp([],Y,Fun) when is_list(Y) ->
    true;
deep_cmp(X,[],Fun) when is_list(X)->
    false;
deep_cmp([X|Xs],[X|Ys],Fun) ->
    deep_cmp(Xs,Ys,Fun);
deep_cmp([X|Xs],[Y|Ys],Fun) ->
    case deep_cmp(X,Y,Fun) of
	equal ->
	    deep_cmp(Xs,Ys,Fun);
	Res ->
	    Res
    end;
deep_cmp(X,Y,Fun) ->
    Fun(X,Y).

wc_cmp(X,X) ->
    equal;
wc_cmp('*',Y) ->
    false;
wc_cmp(X,'*') ->
    true;
wc_cmp(X,Y) ->
    X<Y.

mk_wildcard(MOI) ->
    MOI1=lists:reverse(MOI),
    mk_wildcard(MOI1,[],[MOI1]).

mk_wildcard([{X,Y}|MOI],HD,Acc) ->
    mk_wildcard(MOI,HD++[{X,'*'}],[HD++[{X,'*'}|MOI]|Acc]);
mk_wildcard([],HD,Acc) ->
    lists:reverse([lists:reverse(X)||X<-Acc]).

