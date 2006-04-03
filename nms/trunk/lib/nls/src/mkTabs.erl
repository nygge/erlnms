%%%-------------------------------------------------------------------
%%% File    : mkTabs.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : Create mnesia tables
%%%
%%% Created :  7 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(mkTabs).
-export([text/0]).
-include("text.hrl").

text()->
    mnesia:create_table(text,[{attributes,record_info(fields,text)},
			      {disc_copies,[node()]}]).


