%%%-------------------------------------------------------------------
%%% File    : rrdtool_worker.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 23 May 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrdtool_worker).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {port}).

-include("rrdtool.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    {ok,Port}=rrd_lib:open(),
    {ok,#state{port=Port}}.
    
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
handle_call(Request, _From, State) ->
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
handle_info({cmd,Id,{CMD,Pars}}, State) ->
    Res=do_cmd(State#state.port,CMD,Pars),
    rrdtool!{done,CMD,self(),Id,Res},
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

do_cmd(Port,create,Spec) ->
    rrd_lib:create(Port,Spec);

% do_cmd(Port,{dump,File,ToFile}) ->
%     ok;

do_cmd(Port,fetch,{File,CF,Res,Start,Stop}) ->
    rrd_lib:fetch(Port,File,CF,Res,Start,Stop);

do_cmd(Port,graph,{File,Pars}) ->
    rrd_lib:graph(Port,File,Pars);

do_cmd(Port,info,{File,Type}) ->
    rrd_lib:info(Port,File,Type);

do_cmd(Port,last,{File}) ->
    rrd_lib:last(Port,File);

do_cmd(Port,restore,{Opts,FromFile,ToFile}) ->
    rrd_lib:restore(Port,Opts,FromFile,ToFile);

 % do_cmd(Port,{rrdresize,File,RRA,OP,Rows}) ->
 %     ok;

 % do_cmd(Port,{tune,File,Pars}) ->
 %     ok;

do_cmd(Port,xport,Pars) ->
    rrd_lib:xport(Port,Pars);

do_cmd(Port,CMD,{File,Template,Values}) when CMD==update; CMD==update_async ->
    rrd_lib:update(Port,File,Template,Values).
        
