%%%-------------------------------------------------------------------
%%% File    : ftp_collector.erl
%%% Author  : Anders <anders@local>
%%% Description : Callback module for ftp file collector.
%%%
%%% Created : 11 Jun 2004 by Anders <anders@local>
%%%-------------------------------------------------------------------
-module(ftp_collector).

-behaviour(file_collector).

-record(state,{pid,server,user,passwd,dir,pat}).

-export([start_link/2,
	 init/0,connect/2,get_file_list/1,get_file/3,
	 del_file/2,move_file/3,disconnect/1,terminate/2]).

start_link({Server,User,Passwd,Dir,Pat}=From,To) ->
    file_collector:start_link(?MODULE,From,To).

init() ->
    {ok,#state{}}.

%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
connect({Server,User,Passwd,Dir,Pat},State) ->
    {ok,Pid}=ftp:open(Server),
    ok=ftp:user(Pid,User,Passwd),
    ok=ftp:cd(Pid,Dir),
    State=#state{pid=Pid,server=Server,user=User,passwd=Passwd,
		 dir=Dir,pat=Pat},
    {ok,State}.
%%===========================================================
%% Returns:
%% {ok,Result,State} | {error,Reason,State}
get_file_list(State) ->
    {ok,List}=ftp:nlist(State#state.pid),
    Files=parse_list(List,State#state.pat),
    {ok,Files,State}.

%%===========================================================
%% Returns:
%% {ok,Result,State} | {error,Reason,State}
get_file(File,ToFile,State) ->
    ok=ftp:recv(State#state.pid,File,ToFile),
    {ok,State}.

%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
del_file(File,State) ->
    ok=ftp:delete(State#state.pid,File),
    {ok,State}.

move_file(File,To,State) ->
    ok=ftp:rename(State#state.pid,File,To),
    {ok,State}.
    
%%===========================================================
%% Returns:
%% {ok,State} | {error,Reason,State}
disconnect(State) ->
    ok=ftp:close(State#state.pid),
    {ok,State}.

%%===========================================================
%% Returns:
%% any
terminate(Reason,State) ->
    ok.

parse_list(List,Pat) ->
    Files=string:tokens(List,"\n\r"),
    lists:sort(Files).
