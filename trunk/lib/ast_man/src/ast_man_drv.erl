%%%-------------------------------------------------------------------
%%% File    : ast_manager.erl
%%% Author  : anders <anders@>
%%% Description : 
%%%
%%% Created : 20 Feb 2006 by anders <anders@>
%%%-------------------------------------------------------------------
-module(ast_man_drv).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 send/2
	]).


%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-define(SERVER,?MODULE).

-record(state, {state,queue,from,aid=0,sock,rest=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(Request) ->
    gen_server:call(?SERVER,{api,Request,self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok,Sock} = gen_tcp:connect({192,168,1,50},5038,[list]),
    {ok, #state{state=free,
		queue=queue:new(),
		sock=Sock}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({api,Request,Pid}, _From, State) when State#state.state==free ->
    Aid=State#state.aid,
    Req=add_aid(Request,Aid),
    send(State#state.sock,Req),
    {reply, Aid, State#state{from=Pid,
			     state=busy,
			     aid=Aid+1}};
handle_call({api,Request,Pid}, _From, State) when State#state.state==busy ->
    Aid=State#state.aid,
    Req=add_aid(Request,Aid),
    {reply, Aid,State#state{queue=queue:in({Pid,Req},
					   State#state.queue),
			    aid=Aid+1}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp,_Sock,"Asterisk Call Manager"++_More}, State) ->
    {noreply, State};
handle_info({tcp,_Sock,Data}, State) ->
    {Msgs,Rest}=parse(State#state.rest ++ Data),
    lists:foreach(fun (Msg) ->
			  io:format("Got ~p~n",[Msg])
		  end,Msgs),
    {noreply, State#state{rest=Rest}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
add_aid(Request,Aid) ->
    [Request,"ActionId: ",integer_to_list(Aid),"\r\n\r\n"].

send(Sock,Pack) ->
    io:format("sending ~p~n",[Pack]),
    case gen_tcp:send(Sock,Pack) of
 	ok ->
 	    ok;
 	{error,Error} ->
 	    Error
    end.

parse(Data) ->
    parse_it(Data,[]).

parse_it(Data,Acc) ->
    case split(Data) of
	{nothing,Rest} ->
	    {lists:reverse(Acc),Rest};
	{Msg,More} ->
	    Msg1=msg_type(Msg),
	    parse_it(More,[Msg1|Acc])
    end.

split(Data) ->
    case string:str(Data,"\r\n\r\n") of
	Pos when Pos>0 ->
	    Msg=string:substr(Data,1,Pos-1),
	    Msg1=string:tokens(Msg,"\r\n"),
	    Msg2=[split_line(L)|| L<- Msg1],
	    More=string:substr(Data,Pos+4),
	    {Msg2,More};
	_NotFound ->
	    {nothing,Data}
    end.
%%

split_line(L) ->
    Pos=string:str(L,": "),
    Lbl=string:substr(L,1,Pos-1),
    Value=string:substr(L,Pos+2),
    {Lbl,Value}.
    
msg_type([{"Event",_EventType}|_More]=Msg) ->
    {event,Msg};
msg_type([{"Response",_EventType}|_More]=Msg) ->
    {response,Msg}.
