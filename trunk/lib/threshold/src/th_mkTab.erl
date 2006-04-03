%%%-------------------------------------------------------------------
%%% File    : th_mkTab.erl
%%% Created : 23 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Create mnesia tables for the Threshold application.
%%% @end
%%%-------------------------------------------------------------------
-module(th_mkTab).

-include("threshold.hrl").

-export([th_dest/0,
	 th_cmd/0,
	 th_prev/0,
	 th_threshold/0]).

%% @spec th_dest() -> res
%% @doc Create table th_dest.
%% end
th_dest() ->
    mnesia:create_table(th_dest,[{attributes,record_info(fields,th_dest)},
				   {disc_copies,[node()]},{type,set}]).

%% @spec th_cmd() -> Res
%% @doc Create table th_cmd.
%% end
th_cmd() ->
    mnesia:create_table(th_cmd,[{attributes,record_info(fields,th_cmd)},
				   {disc_copies,[node()]},{type,set}]).
%% @spec th_prev() -> Res
%% @doc Create table th_prev.
%% end
th_prev() ->
    mnesia:create_table(th_prev,[{attributes,record_info(fields,th_prev)},
				   {disc_copies,[node()]},{type,set}]).

%% @spec th_threshold() -> Res
%% @doc Create table th_threshold.
%% end
th_threshold() ->
    mnesia:create_table(th_threshold,[{attributes,
				       record_info(fields,th_threshold)},
				      {disc_copies,[node()]},{type,bag}]).

