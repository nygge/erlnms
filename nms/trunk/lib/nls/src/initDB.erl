%%% File    : initDB.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%% Created :  7 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>

-module(initDB).
-export([mkTab/0,load/1]).
-include("text.hrl").

mkTab()->
    mnesia:create_table(text,[{attributes,record_info(fields,text)},
			      {disc_copies,[node()]}]).


load(File)->
    {ok,L}=file:consult(File),
    L1=lists:map(fun ({Type,Lang,Start,Strings})->
			 mkT(Type,Lang,Start,Strings)
		 end, L),
    lists:foreach(fun (X) -> 
			  insert(X)
		  end, L1).

mkT(Type,Lang,N,[S|Ss]) when is_atom(Type),is_atom(lang),is_integer(N) ->
    [{text,{Type,N,Lang},S}|mkT(Type,Lang,N+1,Ss)];
mkT(_Type,_Lang,_N,[]) ->
    [].

insert(L) ->
    F=fun() ->
	      lists:foreach(fun (S) ->
				    mnesia:write(S)
			    end,L)
      end,
    mnesia:transaction(F).

