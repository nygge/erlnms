%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : rate_prefix.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Number prefix analysis.
%%% @end 
%%% Created : 23 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rate_prefix).

%% API
-export([insert/3,
	 lookup/2,
	 new/0]).

-record(node,{children}).
-record(leaf,{data}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec insert(Prefix::list(),Data::term(),Tree::tree()) -> tree()
%% @doc Insert a new prefix. 
%% @end
%%--------------------------------------------------------------------
insert(Prefix,Data,Tree) ->
    do_insert(Prefix,Data,Tree).

%%--------------------------------------------------------------------
%% @spec lookup(Number::list(),Tree::tree()) -> {Rest::list(),Data::term()}
%% @doc Lookup the prefix for Number. Returns the remaining digits and
%% the Data associated with the prefix.
%% @end
%%--------------------------------------------------------------------
lookup(Number,Tree) ->
    do_lookup(Number,Tree).

%%--------------------------------------------------------------------
%% @spec new() -> tree()
%% @doc Create a new empty prefix tree.
%% @end
%%--------------------------------------------------------------------
new() ->
    empty.
%%====================================================================
%% Internal functions
%%====================================================================
do_insert([First|Rest],Data,empty) ->
    T=do_insert(Rest,Data,new()),
    D=list_to_tuple(lists:duplicate(10,empty)),
    D1=setelement(to_index(First),D,T),
    #node{children=D1};
do_insert([First|Rest],Data,Tree) ->
    Index=to_index(First),
    T=do_insert(Rest,Data,element(Index,Tree#node.children)),
    D1=setelement(Index,Tree#node.children,T),
    Tree#node{children=D1};    
do_insert([],Data,empty) ->
    #leaf{data=Data}.

do_lookup([_First|_Rest],empty) ->
    notfound;
do_lookup(More,Leaf) when is_record(Leaf,leaf) ->
    {More,Leaf#leaf.data};
do_lookup([First|Rest],Tree) ->
    Index=to_index(First),
    do_lookup(Rest,element(Index,Tree#node.children)).

to_index("0") -> 
    10;
to_index(C) ->
    C-$0.

