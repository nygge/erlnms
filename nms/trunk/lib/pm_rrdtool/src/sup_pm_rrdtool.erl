%%%-------------------------------------------------------------------
%%% File    : sup_pm_rrdtool.erl
%%% Created :  8 Jul 2005 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2005-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Supervisor for PM database backend based on RRDtool.
%%% @end
%%%-------------------------------------------------------------------
-module(sup_pm_rrdtool).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @private
%%--------------------------------------------------------------------
init([]) ->
    AChild = {pm_rrd_config,{pm_rrd_config,start_link,[]},
	      permanent,2000,worker,[pm_rrd_config]},
    {ok,{{one_for_all,0,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
