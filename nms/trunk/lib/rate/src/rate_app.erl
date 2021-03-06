%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : rate_app.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc 
%%% @end 
%%% Created : 22 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rate_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%% @doc This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case rate_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
