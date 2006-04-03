%%%-------------------------------------------------------------------
%%% File    : time.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @copyright 2003-2006 Anders Nygren
%%% @version {@vsn}
%%% @doc Utility library with time related functions.
%%% @end
%%% Created : 30 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
%% @type datetime() = {{Year::integer(),Month::integer(),Day::integer()},{Hour::integer(),Minute::integer(),Second::integer()}}.
%% Datetime as returned by erlang:localtime/0 and erlang:universaltime/0.
%% @type duration() = {Unit::unit(),Num::integer()}.
%% @type unit() = sec|min|hour|day|week|month|year
%% @type epoch() = integer().
%% Number of seconds since 1970-01-01-00:00:00.

-module(time).

-export([
 	 datetime_to_epoch/1,
 	 epoch_to_datetime/1,
 	 time_calc/2,
 	 duration_div/2,
 	 duration_to_binary/1,
 	 duration_to_seconds/1,
 	 seconds_to_duration/1
 	]).

-define(EPOCH,{{1970,1,1},{0,0,0}}).
-define(MINUTE,60).
-define(HOUR,60*?MINUTE).
-define(DAY,24*?HOUR).
-define(WEEK,7*?DAY).
-define(MONTH,31*?DAY).
-define(YEAR,366*?DAY).

%% @spec datetime_to_epoch(DateTime::datetime()) -> epoch()
%% @doc Convert datetime() to epoch().
datetime_to_epoch({{_Y,_Mo,_D},{_H,_Mi,_S}}=DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime)-
	calendar:datetime_to_gregorian_seconds(?EPOCH).

%% @spec epoch_to_datetime(Epoch::epoch()) -> datetime()
%% @doc Convert epoch() to datetime().
epoch_to_datetime(T) ->
    calendar:gregorian_seconds_to_datetime(T+calendar:datetime_to_gregorian_seconds(?EPOCH)).

%% @spec time_calc(DateTime::datetime(),Dur::duration()) -> datetime()
%% @doc Add a duration() to datetime().
time_calc(DateTime,Dur) ->
    TSecs=calendar:datetime_to_gregorian_seconds(DateTime),
    DSecs=duration_to_seconds(Dur),
    calendar:gregorian_seconds_to_datetime(TSecs+DSecs).

%% @spec duration_to_binary(Dur::duration()) -> binary()
%% @doc Convert a duration() to seconds and then convert that to a binary().
duration_to_binary(Dur) when is_integer(Dur) ->
    list_to_binary(integer_to_list(Dur));
duration_to_binary(Dur) ->
    duration_to_binary(duration_to_seconds(Dur)).

%% @spec duration_to_seconds(N::duration) -> integer()
%% @doc Convert duration() to seconds.
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

%% @spec seconds_to_duration(S::integer()) -> duration()
%% @doc Convert seconds() to duration.
seconds_to_duration(S) ->
    T=[{year,?YEAR},{month,?MONTH},{week,?WEEK},{day,?DAY},
       {hour,?HOUR},{min,?MINUTE},{sec,1}],
    {Type,Factor}=hd(lists:filter(fun ({_L,K}) ->
					  (S rem K) == 0
				  end,
				  T)),
    {Type,S div Factor}.
    

%% @spec duration_div(X::duration(),Y::duration) -> integer()
%% @doc Divide two durations. Exits if not divisable.
duration_div(X,Y) ->
    X1=duration_to_seconds(X),
    Y1=duration_to_seconds(Y),
    case X1 rem Y1 of
	0 ->
	    X1 div Y1;
	_N ->
	    exit({?MODULE,duration_div,not_dividable,X,Y})
    end.
