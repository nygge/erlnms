%%%-------------------------------------------------------------------
%%% File    : lists2.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  9 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(lists2).

-export([find_key_pos/3, intersperse/2, union/2, 
	 zip/2,zip/3]).

find_key_pos(Key,Pos,List)->
    find_key_pos(Key,Pos,List,1).

find_key_pos(Key,Pos,[T|Ts],N)->
    case element(Pos,T) of
	Key -> {value,N};
	_ -> find_key_pos(Key,Pos,Ts,N+1)
    end;

find_key_pos(Key,Pos,[],N)->
    false.

intersperse(Sep,[]) ->
    [];
intersperse(Sep,[X]=A) ->
    A;
intersperse(Sep,[X|Xs]) ->
    [X,Sep|intersperse(Sep,Xs)].

union(L1,L2) when is_list(L1), is_list(L2) ->
    lists:foldl(fun (F,Acc) ->
			case lists:member(F,Acc) of
			    false -> [F|Acc];
			    true -> Acc
			end
		end,
		L2,
		L1).

zip(L1,L2) when is_list(L1), is_list(L2) ->
    lists:reverse(zip2(L1,L2,[])).

zip2(L1,L2,Acc) when L1==[];L2==[]->
    Acc;

zip2([H1|L1],[H2|L2],Acc) ->
    zip2(L1,L2,[{H1,H2}|Acc]).

zip(L1,L2,L3) when is_list(L1), is_list(L2), is_list(L3) ->
    lists:reverse(zip3(L1,L2,L3,[])).

zip3(L1,L2,L3,Acc) when L1==[];L2==[];L3==[]->
    Acc;

zip3([H1|L1],[H2|L2],[H3|L3],Acc) ->
    zip3(L1,L2,L3,[{H1,H2,H3}|Acc]).

