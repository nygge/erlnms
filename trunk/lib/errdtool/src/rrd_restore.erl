%%%-------------------------------------------------------------------
%%% File    : rrd_restore.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_restore).

-export([do_restore/4]).

do_restore(Port,Opts,FromFile,ToFile) ->
    CMD=create_cmd(Opts,FromFile,ToFile),
    rrd_lib:do_cmd(Port,CMD).

create_cmd(Opts,FromFile,ToFile) ->
    [<<"restore ">>,
     opts_to_binary(Opts),
     <<FromFile>>, <<" ">>,
     <<ToFile>>,
     <<$\n>>].

opts_to_binary([range_check]) ->
    <<"-r ">>;
opts_to_binary([]) ->
    <<"">>.
