%%%-------------------------------------------------------------------
%%% File    : rrd_last.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2003-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%% @private
%%%
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_last).

-export([do_last/2]).

do_last(Port,File) ->
    CMD=create_cmd(File),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,R} ->
	    time:epoch_to_datetime(list_to_integer(R));
	Error ->
	    Error
    end.

create_cmd(File) ->
    [<<"last ">>,
     list_to_binary(File++" "),
     <<$\n>>].

