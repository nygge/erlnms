%%%-------------------------------------------------------------------
%%% File    : rrd_update.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2003-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%% @private
%%%
%%% Created : 27 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rrd_update).

-export([do_update/4]).

do_update(Port,File,Template,Values) ->
    CMD=create_cmd(File,Template,Values),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,nothing} ->
	    TS=get_time_stamps(Values),
	    {ok,TS};
	Error ->
	    Error
    end.

create_cmd(File,Template,Values) ->
    [<<"update ">>,
     list_to_binary(File++" "),
     template_to_binary(Template),
     values_to_binary(Values),
     <<$\n>>
    ].

template_to_binary([]) ->
    <<>>;
template_to_binary(Templ) ->
    [<<"-t ">>,
     rrd_lib_utils:vals_to_binary(Templ,":"),
     <<" ">>
    ].

values_to_binary(Values) ->
    lists:map(fun(X)->
 		      value_to_binary(X)
 	      end, Values).

value_to_binary({n,Values}) ->
    [<<"N:">>,
     rrd_lib_utils:vals_to_binary(Values,":")
    ];

value_to_binary({N,Values}) when is_integer(N)->
    [rrd_lib_utils:val_to_binary(N),
     <<":">>,
     rrd_lib_utils:vals_to_binary(Values,":")].

get_time_stamps(L) ->
    lists:sort(lists:map(fun ({TS,Vals}) ->
				 TS
			 end, L)).
