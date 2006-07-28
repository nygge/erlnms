%%%-------------------------------------------------------------------
%%% File    : emx5000_job.erl
%%% Author  : Anders <anders@local>
%%% Description : 
%%%
%%% Created : 20 Jul 2004 by Anders <anders@local>
%%%-------------------------------------------------------------------
-module(emx5000_job).

-export([graph/0]).

-include("dfe.hrl").

graph() ->
    G=digraph:new().
%%    digraph:add_vertex(G,start,#pn_rec{proc=fun ()}).
