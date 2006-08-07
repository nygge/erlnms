%%%-------------------------------------------------------------------
%%% File    : pm_config.erl
%%% Created : 25 May 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Configuration data for pm_rrdtool.
%%% @end
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

	 new_store_inst/1,
	 get_store_inst/1,
	 get_all_store_insts/0,
	 delete_store_inst/1,

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

%% Store_Inst
new_store_inst(Rec) when is_record(Rec,pm_rrd_inst) ->
    gen_server:call(?SERVER,{new,Rec},infinity).

delete_store_inst(Key) ->
    gen_server:call(?SERVER,{delete,pm_rrd_inst,Key},infinity).

get_store_inst(Key) ->
    gen_server:call(?SERVER,{get,pm_rrd_inst,Key},infinity).

get_all_store_insts() ->
    gen_server:call(?SERVER,{get_all,pm_rrd_inst},infinity).

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
handle_call({new,Record}, _From, State) ->
    Reply=insert_tab(Record),
    {reply,Reply,State};

handle_call({delete,Tab,Key}, _From, State) ->
    Reply=delete(Tab,Key),
    {reply,Reply,State};

handle_call({get,Tab,Key}, _From, State) ->
    Reply=read_tab(Tab,Key),
    {reply,Reply,State};

handle_call({get_all,Tab}, _From, State) ->
    Reply=get_all(Tab),
    {reply,Reply,State};

handle_call({id_to_file,MOI,MeasType}, _From, State) ->
    case read_tab(pm_rrd_inst,{MOI,MeasType}) of
	[#pm_rrd_inst{name=_Name,file=File}] ->
	    {reply,{found,File},State};
	_Other ->
	    {reply,not_found,State}
    end;

handle_call({file_to_id,_File}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call(_Any, _From, State) ->
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

delete(pm_store_inst,Key) ->
    delete_row(pm_store_inst,Key).

read_tab(Table,Key) ->
    mnesia:dirty_read({Table,Key}).
    
write(Record) ->
    F=fun() ->
	      mnesia:write(Record)
      end,
    mnesia:transaction(F).

get_all(Tab) ->
    [hd(mnesia:dirty_read(Tab,Key)) || Key <- mnesia:dirty_all_keys(Tab)].

delete_row(Tab,Key) ->
    F=fun() ->
	      mnesia:delete({Tab,Key})
      end,
    mnesia:transaction(F).
    
