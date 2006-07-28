%%%-------------------------------------------------------------------
%%% File    : dfe.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 23 Sep 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(dfe).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("dfe.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {graph}).

-define(SERVER,?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Graph,Parser,File,Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Graph,Parser,File,Args], []).

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
init([Graph,Parser,File,Args]) ->
    {ok,G}=load_graph(Graph),
    process_file(File,Parser,G,Args),
    {ok, #state{graph=G}}.

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

load_graph(Graph) ->
    Graph:graph().

process_file(File,Parser,Graph,Args) ->
    ok=pre_call(Graph,Args),
    Parser:process(File, 
		   fun (R) ->
			   process_one(Graph,R)
		   end),
    ok=post_call(Graph).
    
pre_call(G,Args) ->
    Vs=digraph:vertices(G),
    case catch call_init(G,Vs,Args) of
	ok ->
	    pre_call(G,Vs,Args);
	{'EXIT',Reason} ->
	    {error,Reason}
    end.

call_init(_G,_Vs,_Args) ->
    ok.

pre_call(G,Vs,Args) ->
    lists:foreach(fun ({V,L}) ->
			  case (L#pn_rec.pre)(Args) of
			      {ok,State} ->
				  digraph:add_vertex(G,V,L#pn_rec{state=State});
			      {error,Reason} ->
				  throw({V,Reason})
			  end
		  end,Vs).

process_one(Graph,Data) ->
    process_one(Graph,start,Data).

process_one(Graph,Vertex,Data) ->
    {_,Conf}=digraph:vertex(Graph,Vertex),
    State=Conf#pn_rec.state,
    Fun=Conf#pn_rec.proc,
    case catch Fun(Data,State) of
	{ok,Path,NewData,NewState} when Path==delete; Path==suspend->
	    update_status(Graph,Vertex,Conf,Path,NewState),
	    process_one(Graph,Path,NewData);
	{ok,Path,NewData,NewState} ->
	    update_status(Graph,Vertex,Conf,Path,NewState),
	    Vs=[V||{_Id,_V,V,P}<-digraph:out_edges(Graph,Vertex),
		  P==Path],
	    lists:foreach(fun (V) ->
				  process_one(Graph,V,NewData)
			  end, Vs);
	{error,Reason} ->
	    log_error(Vertex,Data,Reason);
	{'EXIT',Reason} ->
	    log_error(Vertex,Data,Reason);
	Ops ->
	    log_error(Vertex,Data,Ops)
    end.

update_status(Graph,Vertex,Conf,Path,State) ->
    OutC=incr_out(Conf#pn_rec.outcount,Path),
    InC=Conf#pn_rec.incount+1,
    NewConf=Conf#pn_rec{outcount=OutC,incount=InC,state=State},
    digraph:add_vertex(Graph,Vertex,NewConf).

incr_out(Cs,Path) ->
    case lists:keysearch(Path,1,Cs) of
	{value,{Path,Count}} ->
	    lists:keyreplace(Path,1,Cs,{Path,Count+1});
	false ->
	    [{Path,1}|Cs]
    end.

post_call(G) ->
    Vs=digraph:vertices(G),
    case catch post_call1(Vs) of
	ok ->
	    ok;
	{'EXIT',Reason} ->
	    {error,Reason}
    end.

post_call1(Vs) ->
    lists:foreach(fun ({V,L}) ->
			  case catch (L#pn_rec.post)(L#pn_rec.state) of
			      ok ->
				  ok;
			      {error,Reason} ->
				  throw({V,Reason});
			      {'EXIT',_Reason} ->
				  ignore
			  end
		  end,Vs).

log_error(_Vertex,_Data,_Reason) ->
    ok.
