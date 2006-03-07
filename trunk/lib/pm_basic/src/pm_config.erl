%%%-------------------------------------------------------------------
%%% File    : pm_config.erl
%%% Author  : Anders <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 25 May 2004 by Anders <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_config).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("pm_config.hrl").

-define(SERVER,?MODULE).

-define(MINUTE,60).
-define(HOUR,60*?MINUTE).
-define(DAY,24*?HOUR).
-define(WEEK,7*?DAY).
-define(MONTH,31*?DAY).
-define(YEAR,366*?DAY).


%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,start_link/1,
	 datetime_to_epoch/1,
	 epoch_to_datetime/1,
	 duration_to_seconds/1,
	 get_db_backend/2,
	 get_duration/1,
	 get_events/2
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

get_db_backend(MOI,MOC) ->
    gen_server:call(?SERVER,{get_db_backend,MOI,MOC},infinity).

get_events(MOI,MOC) ->
    gen_server:call(?SERVER,{get_events,MOI,MOC},infinity).

get_duration(Dur) ->
    gen_server:call(?SERVER,{get_duration,Dur},infinity).

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
    {reply,get_backend(MOI,MOC),State};

handle_call({get_events,MOI,MOC}, _From, State) ->
    Res=case get_backend(MOI,MOC) of
	    {found,Module} ->
		Module:get_events(MOI,MOC);
	    not_found ->
		not_found
	end,
    {reply,Res,State};

handle_call({get_duration,D}, _From, State) when is_atom(D) ->
    [DurRec]=pm_rrd_config:get(pm_duration,D),
    {reply,DurRec#pm_duration.value,State};
handle_call({get_duration,D}, _From, State) when is_tuple(D)->
    {reply,D,State}.

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

get_backend(MOI,MOC) ->
    {found,pm_rrd_access};

get_backend(MOI,MOC) ->
    case read_tab(pm_db_backend,{MOI,MOC}) of
	[DB] ->
	    {found,DB#pm_db_backend.db};
	[] ->
	    not_found
    end.

read_tab(Table,Key) ->
    mnesia:dirty_read({Table,Key}).
    
%% write(Record) ->
%%     F=fun() ->
%% 	      mnesia:write(Record)
%%       end,
%%     mnesia:transaction(F).


datetime_to_epoch({{_Y,_Mo,_D},{_H,_Mi,_pmS}}=DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime)-
	calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

epoch_to_datetime(T) ->
    calendar:gregorian_seconds_to_datetime(T+calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).


duration_to_seconds(N) when is_integer(N) ->
    N;
duration_to_seconds({sec,N}) ->
    N;
duration_to_seconds({min,N}) ->
    ?MINUTE*N;
duration_to_seconds({hour,N}) ->
    ?HOUR*N;
duration_to_seconds({day,N}) ->
    ?DAY*N;
duration_to_seconds({week,N}) ->
    ?WEEK*N;
duration_to_seconds({month,N}) ->
    ?MONTH*N;
duration_to_seconds({year,N}) ->
    ?YEAR*N.
