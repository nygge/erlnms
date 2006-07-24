%%%-------------------------------------------------------------------
%%% File    : rrdtool_lock.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%%
%%% Created : 18 May 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrdtool_lock).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,lock/3,release/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-define(SERVER,?MODULE).

-record(state, {resources,users}).
-record(resource,{name,lock,locked_by,queue}).
-record(user,{name,pid,resources=[],type,waits_for=0}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid}
%% @doc Starts the locking server.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec lock(Id,Type,Resources) -> ok
%% Id = term()
%% Type = write|read
%% Resources = [Resource]
%% Resource = string()
%% @doc Locks Resources for user Id. 
%% @end
lock(Id,Type,Resources) when Type==write;Type==read ->
    gen_server:cast(?SERVER,{lock,self(),Id,Type,Resources}).

%% @spec release(Id) -> ok
%% Id = term()
%% @doc Releases all Resources locked by user Id. 
%% @end

release(Id) ->
    gen_server:cast(?SERVER,{release,self(),Id}).

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
%% @private
init([]) ->
    Resources=ets:new(resources,[set,protected,{keypos,2}]),
    Users=ets:new(users,[set,protected,{keypos,2}]),
    {ok, #state{resources=Resources,users=Users}}.

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
%% @private
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
%% @private
handle_cast({lock,Pid,Id,Type,Resources}, State) ->
    lock(Pid,Id,Type,Resources,State),
    {noreply,State};

handle_cast({release,Pid,Id}, State) ->
    release(Pid,Id,State),
    {noreply,State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

lock(Pid,Id,Type,Resources,State)->
    ets:insert(State#state.users,#user{name=Id,pid=Pid,
				       resources=Resources,
				       type=Type,
				       waits_for=length(Resources)}),
    lists:foreach(fun (R)->
			  lock1(Pid,Id,Type,R,
				ets:lookup(State#state.resources,R),
				State)
		  end,
		  Resources).

%% Resource is not locked, grant it.
lock1(Pid,Id,Type,Resource,[],State) ->
    ets:insert(State#state.resources,#resource{name=Resource,
					 lock=Type,
					 locked_by=[Id],
					 queue=[]}),
    grant(Pid,Id,Resource,State);

%% Resource is read locked, grant it.
lock1(Pid,Id,read,Resource,[#resource{name=Resource,lock=read,locked_by=L,queue=_Q}],State) ->
    ets:insert(State#state.resources,#resource{name=Resource,
					       lock=read,
					       locked_by=[Id|L],
					       queue=[]}),
    grant(Pid,Id,Resource,State);

% %% Resource is read locked, request is for write -> wait
% lock1(Pid,Id,write,[#resource{name=R,lock=read,locked_by=L,queue=Q}],R,State) ->
%     ets:insert(State#state.resources,#resource{name=R,
% 					       lock=read,
% 					       locked_by=L,
% 					       queue=Q++[{Id,write}]});

% lock1(Pid,Id,read,[#resource{name=R,lock=write,locked_by=L,queue=Q}],R,State) ->
%     ets:insert(State#state.resources,#resource{name=R,
% 					       lock=write,
% 					       locked_by=L,
% 					       queue=Q++[{Id,read}]});

% lock1(Pid,Id,write,[#resource{name=R,lock=write,locked_by=L,queue=Q}],R,State) ->
%     ets:insert(State#state.resources,#resource{name=R,
% 					       lock=write,
% 					       locked_by=L,
% 					       queue=Q++[{Id,write}]}).

lock1(_Pid,Id,RMode,R,[#resource{name=R,lock=LMode,locked_by=L,queue=Q}],State) 
  when RMode==write,LMode==read;
       RMode==read,LMode==write;
       RMode==write,LMode==write ->
    ets:insert(State#state.resources,#resource{name=R,
					       lock=LMode,
					       locked_by=L,
					       queue=Q++[{Id,RMode}]}).

release(_Pid,Id,State) ->
    release1(ets:lookup(State#state.users,Id),State),
    ets:delete(State#state.users,Id).

release1([],_State)->
    ok;
release1([#user{name=Id,pid=Pid,resources=Resources,type=_,waits_for=_}],State) ->
    lists:foreach(fun (R)->
			  rel_lock(ets:lookup(State#state.resources,R),
				   Id,State)
		  end,
		  Resources).

rel_lock([#resource{name=Name,lock=_Lock,locked_by=Id,queue=[]}],Id,State) ->
    ets:delete(State#state.resources,Name);

rel_lock([#resource{name=Name,lock=_Lock,locked_by=[Id],queue=[]}],Id,State) ->
    ets:delete(State#state.resources,Name);

rel_lock([#resource{name=Name,lock=_Lock,locked_by=Id,queue=Q}],Id,State) ->
    ets:insert(State#state.resources,#resource{name=Name,lock=none,
					       locked_by=none,
					       queue=Q}),
    grant_queued(Name,Q,State);

rel_lock([#resource{name=Name,lock=_Lock,locked_by=[Id],queue=Q}],Id,State) ->
    ets:insert(State#state.resources,#resource{name=Name,lock=none,
					       locked_by=none,
					       queue=Q}),
    grant_queued(Name,Q,State);

rel_lock([#resource{name=Name,lock=Lock,locked_by=LIds,queue=Q}],Id,State) ->
    case lists:member(Id,LIds) of
	true ->
	    NLIds=lists:delete(Id,LIds),
	    ets:insert(State#state.resources,#resource{name=Name,
						       lock=Lock,
						       locked_by=NLIds,
						       queue=Q});
	false ->
	    case lists:keymember(Id,1,LIds) of
		true ->
		    NQ=lists:keydelete(Id,1,Q),
		    ets:insert(State#state.resources,#resource{name=Name,
							       lock=Lock,
							       locked_by=LIds,
							       queue=NQ});
		false ->
		    ok
	    end
    end.

grant_queued(Resource,[{_User,read}|_Us]=Q,State) ->
    grant_readers(Resource,Q,[],State);
grant_queued(Resource,[{User,write}|Us],State) ->
    grant_write(Resource,User,Us,State).

grant_readers(Resource,[{U,read}|More],Readers,State) ->
    [User]=ets:lookup(State#state.users,U),
    grant(User#user.pid,User#user.name,Resource,State),
    grant_readers(Resource,More,[User#user.name|Readers],State);
grant_readers(Resource,[{_U,write}|_More]=Q,Readers,State) ->
    ets:insert(State#state.resources,#resource{name=Resource,
					       lock=read,
					       locked_by=Readers,
					       queue=Q});
grant_readers(Resource,[]=Q,Readers,State) ->
    ets:insert(State#state.resources,#resource{name=Resource,
					       lock=read,
					       locked_by=Readers,
					       queue=Q}).

grant_write(Resource,UserId,Queue,State) ->
    [User]=ets:lookup(State#state.users,UserId),
    grant(User#user.pid,User#user.name,Resource,State),
    ets:insert(State#state.resources,#resource{name=Resource,
					       lock=write,
					       locked_by=UserId,
					       queue=Queue}).

grant(_Pid,User,_Resource,State) ->
    grant(ets:lookup(State#state.users,User),State).

grant([#user{name=Id,pid=Pid,resources=_,type=_,waits_for=1}=UserData],State) ->
    Pid!{locked,Id},
    ets:insert(State#state.users,UserData#user{waits_for=0});
grant([#user{name=_Id,pid=_Pid,resources=_,type=_,waits_for=N}=UserData],State) ->
    ets:insert(State#state.users,UserData#user{waits_for=N-1}).
