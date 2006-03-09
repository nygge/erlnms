%%%-------------------------------------------------------------------
%%% File    : time.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(time).

-export([
 	 datetime_to_epoch/1,
 	 epoch_to_datetime/1,
 	 time_calc/2,
 	 duration_div/2,
 	 duration_to_binary/1,
 	 duration_to_seconds/1,
 	 seconds_to_duration/1,
 	]).

-define(EPOCH,{{1970,1,1},{0,0,0}}).
-define(MINUTE,60).
-define(HOUR,60*?MINUTE).
-define(DAY,24*?HOUR).
-define(WEEK,7*?DAY).
-define(MONTH,31*?DAY).
-define(YEAR,366*?DAY).

datetime_to_epoch({{Y,Mo,D},{H,Mi,S}}=DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime)-
	calendar:datetime_to_gregorian_seconds(?EPOCH).

epoch_to_datetime(T) ->
    calendar:gregorian_seconds_to_datetime(T+calendar:datetime_to_gregorian_seconds(?EPOCH)).

time_calc(DateTime,Dur) ->
    TSecs=calendar:datetime_to_gregorian_seconds(DateTime),
    DSecs=duration_to_seconds(Dur),
    calendar:gregorian_seconds_to_datetime(TSecs+DSecs).

duration_to_binary(Dur) when is_integer(Dur) ->
    val_to_binary(Dur);
duration_to_binary(Dur) ->
    val_to_binary(duration_to_seconds(Dur)).

duration_to_seconds(N) when is_integer(N) ->
    N;
duration_to_seconds({sec,N}) ->
    N;
duration_to_seconds({min,N}) ->
    ?MINUTE*N;
duration_to_seconds({hour,N}) ->
    ?HOUR*N;
duration_to_seconds({day,N}) ->
    ?DAY*N;
duration_to_seconds({week,N}) ->
    ?WEEK*N;
duration_to_seconds({month,N}) ->
    ?MONTH*N;
duration_to_seconds({year,N}) ->
    ?YEAR*N.

seconds_to_duration(S) ->
    T=[{year,?YEAR},{month,?MONTH},{week,?WEEK},{day,?DAY},
       {hour,?HOUR},{min,?MINUTE},{sec,1}],
    {Type,Factor}=hd(lists:filter(fun ({L,K}) ->
					  (S rem K) == 0
				  end,
				  T)),
    {Type,S div Factor}.
    

duration_div(X,Y) ->
    X1=duration_to_seconds(X),
    Y1=duration_to_seconds(Y),
    case X1 rem Y1 of
	0 ->
	    X1 div Y1;
	N ->
	    exit({?MODULE,duration_div,not_dividable,X,Y})
    end.
