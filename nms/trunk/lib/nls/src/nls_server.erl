%%%-------------------------------------------------------------------
%%% File    : nls_server.erl
%%% Author  : Anders <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  7 Aug 2004 by Anders <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(nls_server).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,get_string/3,get_strings/2]).

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

get_string(Type,Key,Lang)->
    gen_server:call(?SERVER,{get_string,Type,Key,Lang}).

get_strings(List,Lang)->
    gen_server:call(?SERVER,{get_string,List,Lang}).

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
handle_call({get_string,Type,Key,Lang}, _From, State) ->
    Reply = get_string1(Type,Key,Lang),
    {reply, Reply, State};

handle_call({get_string,List,Lang}, _From, State) ->
    Reply = get_strings1(List,Lang),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({get_string,From,List,Key,Lang}, State) ->
    Res=get_string1(List,Key,Lang),
    From!Res,
    {noreply, State};

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

get_string1(Type,Key,Lang)->
    F=fun () ->
 	      mnesia:read({text,{Type,Key,Lang}})
      end,
    Res=mnesia:transaction(F),

    case Res of
	{atomic,[{text,{Type,Key,Lang},String}]} ->
	    {found,String};
	{atomic,[]} -> 
	    {error,not_found}
    end.

get_strings1(_List,_Lang)->
    {ok,nothing}.
