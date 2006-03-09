%%%-------------------------------------------------------------------
%%% File    : rrd_lib_utils.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_lib_utils).

-export([
 	 vals_to_binary/1,
 	 vals_to_binary/2,
 	 val_to_binary/1
 	]).

vals_to_binary(Vals,Sep) ->
    L=lists2:intersperse(Sep,Vals),
    vals_to_binary(L).

vals_to_binary(Vals) ->
    lists:map(fun(X) ->
		      val_to_binary(X)
	      end,Vals).

val_to_binary(L) when is_list(L)->
    list_to_binary(L);
val_to_binary(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N));
val_to_binary(F) when is_float(F) ->
    list_to_binary(io_lib:format("~g",[F]));
val_to_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
val_to_binary(B) when is_binary(B) ->
    B.
