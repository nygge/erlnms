%%%-------------------------------------------------------------------
%%% File    : disk_collector.erl
%%% Author  : Anders <anders@local>
%%% Description : 
%%%
%%% Created : 10 Jun 2004 by Anders <anders@local>
%%%-------------------------------------------------------------------
-module(disk_collector).

-behaviour(file_collector).

-record(state,{dir,pat}).

-export([start_link/2,start_link/3,
	 init/0,connect/2,get_file_list/1,get_file/3,
	 del_file/2,disconnect/1,terminate/2]).

start_link(From,To) ->
    start_link(From,"*",To).

start_link(From,Pat,To) ->
    file_collector:start_link(?MODULE,{From,Pat},To).

init() ->
    {ok,#state{}}.

%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
connect({Dir,Pat},State) ->
    case filelib:is_dir(Dir) of
	true ->
	    {ok,#state{dir=Dir,pat=Pat}};
	false ->
	    {error,{not_a_dir,Dir},State}
    end.

%%===========================================================
%% Returns:
%% {ok,Result,State} | {error,Reason,State}
get_file_list(State) ->
    Dir=State#state.dir,
    Pat=State#state.pat,
    D=filename:join(Dir,Pat),
    FL=filelib:wildcard(D),
    Files=[filename:basename(File)||File <- FL],
    {ok,lists:sort(Files),State}.

%%===========================================================
%% Returns:
%% {ok,Result,State} | {error,Reason,State}
get_file(File,ToFile,State) ->
    From=filename:join(State#state.dir,File),
    {ok,Chars}=file:copy(From,ToFile),
    {ok,State}.

%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
del_file(File,State) ->
    Dir=State#state.dir,
    FN=filename:join(Dir,File),
    ok=file:delete(FN),
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
