%%%-------------------------------------------------------------------
%%% File    : callback_template.erl
%%% Author  : Anders <anders@local>
%%% Description : Template for callback module for implementing
%%%               file collector for other protocols.
%%%
%%% Created : 11 Jun 2004 by Anders <anders@local>
%%%-------------------------------------------------------------------
-module(callback_template).

-behaviour(file_collector).

-record(state,{dir,pat}).

-export([start_link/2,
	 init/0,connect/2,get_file_list/1,get_file/3,
	 del_file/2,disconnect/1,terminate/2]).

start_link(From,To) ->
    file_collector:start_link(?MODULE,From,To).

init() ->
    {ok,#state{}}.

%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
connect(SourceSpec,State) ->
    {ok,State}.
%%===========================================================
%% Returns:
%% {ok,Result,State} | {error,Reason,State}
get_file_list(State) ->
    {ok,[],State}.

%%===========================================================
%% Returns:
%% {ok,Result,State} | {error,Reason,State}
get_file(File,ToFile,State) ->
    {ok,State}.

%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
del_file(File,State) ->
    {ok,State}.

%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
disconnect(State) ->
    {ok,State}.

%%===========================================================
%% Returns:
%% any
terminate(Reason,State) ->
    ok.
