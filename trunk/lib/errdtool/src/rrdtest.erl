%%%-------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : Anders <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  6 Jul 2005 by Anders <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrdtest).
-export([xport/0]).

xport()->
    rrdtool:xport({[{step,{sec,15}},{start,{{2005,7,6},{15,55,15}}},
		    {'end',{{2005,7,6},{15,55,30}}}],
		   [{in,"/home/anders/src/data/pm/ne/1/ifn/1/interface.rrd",
		     in,'AVERAGE'}],
		   [],
		   [{in,in}]}).
