%%%-------------------------------------------------------------------
%%% File    : rrd_last.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_last).

-export([do_last/2]).

do_last(Port,File) ->
    CMD=create_cmd(File),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,R} ->
	    utils:epoch_to_datetime(list_to_integer(R));
	Error ->
	    Error
    end.

create_cmd(File) ->
    [<<"last ">>,
     list_to_binary(File++" "),
     <<$\n>>].

