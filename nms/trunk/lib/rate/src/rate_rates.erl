%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : rate_rates.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc 
%%% @end 
%%% Created : 23 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rate_rates).

-include("rate.hrl").

%% API
-export([add_rate/1,
	 create_table/1,
	 del_rate/1,
	 get_rate/4]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec add_rate(Rate::rate()) -> ok
%% @doc Add a rate.
%% @end
%%--------------------------------------------------------------------
add_rate(Rate) when is_record(Rate,rate) ->
    ok.

%%--------------------------------------------------------------------
%% @spec create_table(Nodes) -> ok
%% @doc Create tables.
%% @end
%%--------------------------------------------------------------------
create_table(Nodes) ->
    mnesia:create_table(rate,
			[{attributes,
			  record_info(fields,rate)},
			 {type,bag},
			 {disc_copies,Nodes}]).

%%--------------------------------------------------------------------
%% @spec del_rate(Rate::rate()) -> ok
%% @doc Delete a rate.
%% @end
%%--------------------------------------------------------------------
del_rate(Rate) ->
    ok.

%%--------------------------------------------------------------------
%% @spec get_rate(From,To,Start::datetime(),Dur::integer()) -> [Rate]
%% Rate = {Dur::integer(),rate()}
%% @doc Get the rates for a call from From to To starting at Start with 
%% a duration of Dur seconds.
%% @end
%%--------------------------------------------------------------------
get_rate(From,To,{Date,Time}=Start,Dur) ->
    F=fun() ->
	      get_rate1({From,To},Date,Time,Dur)
      end,
    case mnesia:transaction(F) of
	{atomic,[]} ->
	    notfound;
	{atomic,Rates} ->
	    Rates
    end.
%%====================================================================
%% Internal functions
%%====================================================================
get_rate1(Key,Date,Time,Dur) ->
    Rates=get_rate2(Key,Date,Time,Dur,[]),
    lists:reverse(Rates).

get_rate2(Key,Date,Time,Dur,Acc) ->
    Rate=find_rate(Key,Date,Time,Dur).
%%    case more_rates(Rate#rate.time_stop,end_time(Time,Dur)

find_rate(Key,Date,Time,Dur) ->
    DT=rate_day_type:daytype_nt(Date),
    Head=#rate{key=Key,
	       time_start='$1',
	       time_stop='$2',
	       date_start='$3',
	       date_stop='$4',
	       daytype=DT,
	       data='_'},
    Conds=[{'and',
	    {'and',{'>=',{Time},'$1'},{'<',{Time},'$2'}},
	    {'and',{'>=',{Date},'$3'},
	     {'or',{'<',{Date},'$4'},{'==','$4',undefined}}}}
	  ],
    Body=['$_'],
    MS=[{Head,Conds,Body}],
    [Rate]=mnesia:select(rate,MS),
    Rate.

%%end_time({H1,M1,S1},{H2,M2,S2}) ->
    
%% anders@godot:~/src/nms/trunk/lib/rate/src> diff rate_rates.erl '#rate_rates.erl#'
%% 17c17
%% <        get_rate/4]).
%% ---
%% >        get_rate/5]).
%% 59c59
%% <             get_rate1({From,To},Date,Time,Dur)
%% ---
%% >             get_rate({From,To},Date,Time,Dur)
%% 70,71c70,71
%% < get_rate1(Key,Date,Time,Dur) ->
%% <     Rates=get_rate2(Key,Date,Time,Dur,[]),
%% ---
%% > get_rate(Key,Date,Time,Dur) ->
%% >     Rates=get_rate1(Key,Date,Time,Dur,[]),
%% 74,76c74,76
%% < get_rate2(Key,Date,Time,Dur,Acc) ->
%% <     Rate=find_rate(Key,Date,Time,Dur).
%% < %%    case more_rates(Rate#rate.time_stop,end_time(Time,Dur)
%% ---
%% > get_rate1(Key,Date,Time,Dur,Acc) ->
%% >     Rate=find_rate(Key,Date,Time,Dur),
%% >     case more_rates(Rate#rate.time_stop,end_time(Time,Dur)
%% 85c85
%% <              daytype=DT,
%% ---
%% >              daytype=DayType,
%% 88,90c88,95
%% <           {'and',{'>=',{Time},'$1'},{'<',{Time},'$2'}},
%% <           {'and',{'>=',{Date},'$3'},
%% <            {'or',{'<',{Date},'$4'},{'==','$4',undefined}}}}
%% ---
%% >           {'and',
%% >            {'>=',{Time},'$1'},
%% >            {'<',{Time},'$2'}},
%% >           {'and',
%% >            {'>=',{Date},'$3'},
%% >            {'or',
%% >             {'<',{Date},'$4'},
%% >             {'==','$4',undefined}}}}
%% 97c102
%% < %%end_time({H1,M1,S1},{H2,M2,S2}) ->
%% ---
%% > end_time({H1,M1,S1},{H2,M2,S2}) ->
%% anders@godot:~/src/nms/trunk/lib/rate/src>      
