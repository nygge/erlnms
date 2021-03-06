%%%-------------------------------------------------------------------
%%% File    : sup_pm_basic.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Jun 2004 by Anders Nygren <anders.nygren@gmail.com>
%%% @private
%%%-------------------------------------------------------------------
-module(sup_pm_basic).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    {ok,{{one_for_all, 5, 30},
	 [{pm_raw_data, {pm_raw_data, start_link, [{local,pm_raw_data}]},
	   transient, 5000, worker, [pm_raw_data]}
	  ,{pm_stored,{pm_stored,start_link,[]},
	    transient,5000,worker,[pm_stored]}
 	  ,{pm_data, {pm_data, start_link, [{local,pm_data}]},
	    transient, 5000, worker, [pm_data]}
	  ,{pm_config, {pm_config, start_link, [{local,pm_config}]},
	    transient, 5000, worker, [pm_config]} 
	  | get_backends()
	 ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

get_backends() ->
    [{sup_pm_rrd,{sup_pm_rrdtool,start_link,[]},
      transient,5000,supervisor,[sup_pm_rrdtool]}
%%     ,{sup_pm_rdbms,{sup_pm_rdbms,start_link,[]},
%%       transient,5000,supervisor,[sup_pm_rdbms]}
    ].
