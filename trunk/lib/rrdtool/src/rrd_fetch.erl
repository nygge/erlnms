%%%-------------------------------------------------------------------
%%% File    : rrd_fetch.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2004-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc 
%%% @end
%%% @private
%%% Created : 25 Jan 2004 by Anders Nygren
%%%-------------------------------------------------------------------
%% %@private

-module(rrd_fetch).

-export([do_fetch/6]).

do_fetch(Port,File,CF,Res,latest,latest) ->
    Esecs=time:datetime_to_epoch(calendar:universal_time()),
    RSecs=time:duration_to_seconds(Res),
    Time=time:epoch_to_datetime((Esecs div RSecs) * RSecs),
    do_fetch(Port,File,CF,Res,Time,Time);

do_fetch(Port,File,CF,Res,Start,Stop) ->
    RSecs=time:duration_to_seconds(Res),
    StartSec=time:datetime_to_epoch(Start),
    StopSec=time:datetime_to_epoch(Stop),
    do_fetch1(Port,File,CF,RSecs,StartSec,StopSec).

do_fetch1(Port,File,CF,Resolution,Start,Stop) ->
    CMD=create_cmd(File,CF,Resolution,Start,Stop),
    case rrd_lib:do_cmd(Port,CMD) of
	{ok,Result} ->
	    parse_res(Result);
	Error ->
	    Error
    end.

create_cmd(File,CF,Res,Start,Stop) ->
    [<<"fetch ">>,
     list_to_binary(File++" "),
     rrd_lib_utils:val_to_binary(CF),
     <<" -r ">>,rrd_lib_utils:val_to_binary(Res),
     <<" -s ">>,rrd_lib_utils:val_to_binary(Start),
     <<" -e ">>,rrd_lib_utils:val_to_binary(Stop),
     <<$\n>>].

parse_res(Res) ->
    Tokens=string:tokens(Res," \n"),
    {HD,TL}=lists:splitwith(fun (X)->
				    lists:last(X)/=$:
			    end,
			    Tokens),
    Ids=lists:map(fun (X)->
 			  list_to_atom(X)
 		  end,
		  tl(HD)),
    NoVals=length(Ids),
    Vals=parse_vals(TL,NoVals),
    {Ids,Vals}.

parse_vals([],_) ->
    [];
parse_vals([TS|More],NoVals) ->
    Time=time:epoch_to_datetime(
 	   list_to_integer(TS-- ":")),
    {CNTs,Rest}=parse_cnt(More,NoVals,[]),
    [{Time,lists:reverse(CNTs)}|parse_vals(Rest,NoVals)].

parse_cnt([H|T],N,Acc) when N>0 ->
    parse_cnt(T,N-1,[mylist_to_float(H)|Acc]);
parse_cnt(L,0,Acc) ->
    {Acc,L}.

mylist_to_float("nan") ->
    unknown;
mylist_to_float(F) ->
    list_to_float(F).
