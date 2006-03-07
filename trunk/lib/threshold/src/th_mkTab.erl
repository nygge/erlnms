%%%-------------------------------------------------------------------
%%% File    : th_mkTab.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 23 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(th_mkTab).

-include("threshold.hrl").

-export([th_dest/0,
	 th_cmd/0,
	 th_prev/0,
	 th_threshold/0]).

th_dest() ->
    mnesia:create_table(th_dest,[{attributes,record_info(fields,th_dest)},
				   {disc_copies,[node()]},{type,set}]).

th_cmd() ->
    mnesia:create_table(th_cmd,[{attributes,record_info(fields,th_cmd)},
				   {disc_copies,[node()]},{type,set}]).
th_prev() ->
    mnesia:create_table(th_prev,[{attributes,record_info(fields,th_prev)},
				   {disc_copies,[node()]},{type,set}]).

th_threshold() ->
    mnesia:create_table(th_threshold,[{attributes,
				       record_info(fields,th_threshold)},
				      {disc_copies,[node()]},{type,bag}]).

