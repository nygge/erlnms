%%%-------------------------------------------------------------------
%%% File    : threshold.erl
%%% Created :  6 Feb 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Threshold server.
%%% @end
%%%-------------------------------------------------------------------
-module(threshold).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("new_pm_data.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,start/0,start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER,?MODULE).
%%====================================================================
%% External functions
%%====================================================================
start() ->
    application:start(?MODULE).

start(Type, StartArgs) ->
    supervisor:start_link({local,sup_threshold},sup_threshold,[]).

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    ok=connect(),
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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% @private
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% @private
%%--------------------------------------------------------------------

handle_info(Data, State) when is_record(Data,new_pm_data) ->
    threshold_worker:spawn(Data),
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% @private
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% @private
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

connect() ->
    Pid=self(),
    Filter=ets:fun2ms(fun (X) when is_record(X,new_pm_data) ->
			      {{send_to,Pid},X};
			  (X)->
			      ignore
			  end),
    ok=pm_data:subscribe(Filter).
