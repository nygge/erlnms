%%%-------------------------------------------------------------------
%%% File    : pm_config.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 25 May 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(pm_rrd_config).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("pm_rrdtool.hrl").

-define(SERVER,?MODULE).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,start_link/1,
	 id_to_file/2,
	 file_to_id/1
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

id_to_file(MO,Meas) ->
    gen_server:call(?SERVER,{id_to_file,MO,Meas}).

file_to_id(File) ->
    gen_server:call(?SERVER,{file_to_id,File}).


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
handle_call({id_to_file,MOI,MeasType}, _From, State) ->
    case read_tab(pm_store_inst,{MOI,MeasType}) of
	[Rec] ->
	    {reply,{found,Rec#pm_rrd_inst.file},State};
	_Other ->
	    {reply,not_found,State}
    end;

handle_call({file_to_id,_File}, _From, State) ->
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

insert_tab(Record) when is_record(Record,pm_rrd_inst) ->
    write(Record).

read_tab(Table,Key) ->
    mnesia:dirty_read({Table,Key}).
    
write(Record) ->
    F=fun() ->
	      mnesia:write(Record)
      end,
    mnesia:transaction(F).
