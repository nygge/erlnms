%%%-------------------------------------------------------------------
%%% File    : fm_mkTab.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 23 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(fm_mkTab).

-include("trap2alarm.hrl").

-export([trap2alarm/0]).

trap2alarm() ->
    mnesia:create_table(trap2alarm,[{attributes,record_info(fields,trap2alarm)},
				   {disc_copies,[node()]},{type,bag}]).
