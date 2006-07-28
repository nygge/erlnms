%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : rate_sup.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc 
%%% @end 
%%% Created : 22 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rate_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(StartArgs) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    AChild = {'AName',{'AModule',start_link,[]},
	      permanent,2000,worker,['AModule']},
    {ok,{{one_for_all,0,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
