%%%-------------------------------------------------------------------
%%% File    : sup_rrdtool.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%% @private
%%%
%%% Created :  3 Feb 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(sup_rrdtool).

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
    RRDTool= {rrdtool,{rrdtool,start_link,[]},
	      permanent,2000,worker,[rrdtool]},
    Locker = {rrdtool_lock,{rrdtool_lock,start_link,[]},
	      permanent,2000,worker,[rrdtool_lock]},
    {ok,{{one_for_all,5,60}, [RRDTool,Locker]}}.

%%====================================================================
%% Internal functions
%%====================================================================
