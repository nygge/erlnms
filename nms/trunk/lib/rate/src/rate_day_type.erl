%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : rate_day_type.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Type of day. Checks the type of day for a specific date 
%%% and location. 
%%% 
%%% Special daytypes are stored in a mnesia table. If the date and 
%%% location is not in the table the type of day returned will be 
%%% depending of the day of the week, i.e. workday or weekend.
%%%
%%% It is possible to have different mappings from day of the week to
%%% type of day depending on the type of calendar used at the location
%%% e.g. mon-fri ar working days in christian countries while sat-wed
%%% are working days in muslim countries.
%%%
%%% Recurrent dates are not (yet) supported, so it is necessary to add
%%% all holidays for each year.
%%% @end 
%%% Created : 22 Apr 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rate_day_type).

-export([add_date/3,
	 create_tables/1,
	 day_type/2,
	 del_date/2]).

-record(day_type,{key,type}).
-record(loc_type,{loc,type}).

%%% spec add_date(Loc::atom(),Date::date(),Type::day_type()) -> ok
%%% @doc Add a daytype for a location.
%%% @end 
add_date(Loc,{Year,Month,Day}=Date,Type) when is_integer(Year),
					      is_integer(Month),
					      is_integer(Day) ->
    F=fun () ->
	      mnesia:write(#day_type{key={Loc,Date},type=Type})
      end,
    case mnesia:transaction(F) of
	{atomic,Res} ->
	    Res;
	Error ->
	    exit(Error)
    end.

%%% spec day_type(Loc::atom(),Date:date()) -> atom()
%%% @doc Get the daytype for Date at Location Loc.
%%% @end 
day_type(Loc,Date) ->
    case lookup_date(Loc,Date) of
	{found,Type} ->
	    Type;
	notfound ->
	    day_of_week(Loc,Date)
    end.

%%% spec create_table(Nodes::[node()]) -> ok
%%% @doc Create the mnesia tables for rate_day_type. Currently the
%%% tables used are day_type and loc_type.
%%% @end 
create_tables(Nodes) ->
    {atomic,ok}=mnesia:create_table(day_type,
				    [{attributes,
				      record_info(fields,day_type)},
				     {disk_copies,Nodes}]),
    {atomic,ok}=mnesia:create_table(loc_type,
				    [{attributes,
				      record_info(fields,loc_type)},
				     {disk_copies,Nodes}]),
    ok.

%%% spec del_date(Loc,Date) ->
%%% @doc Delete the daytype definition for Date at Loc.
%%% @end 
del_date(Loc,Date) ->
    F=fun() ->
	      mnesia:delete({day_type,{Loc,Date}})
      end,
    {atomic,ok}=mnesia:transaction(F),
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

lookup_date(Loc,Date) ->
    F=fun() ->
	      mnesia:read({day_type,{Loc,Date}})
      end,
    case mnesia:transaction(F) of
	{atomic,[#day_type{key=Key,type=Type}]} ->
	    {found,Type};
	{atomic,[]} ->
	    notfound
    end.

day_of_week(Loc,Date) ->
    Day=calendar:day_of_the_week(Date),
    dotw_to_daytype(christian,Day).    
%    dotw_to_daytype(loc_type(Loc),Day).    

loc_type(Loc) ->
    F=fun() ->
	      mnesia:read({loc_type,Loc})
      end,
    case mnesia:transaction(F) of
	{atomic,[#loc_type{loc=Loc,type=Type}]} ->
	    Type;
	{atomic,[]} ->
	    christian
    end.
    
dotw_to_daytype(christian,1) ->
    workday;
dotw_to_daytype(christian,2) ->
    workday;
dotw_to_daytype(christian,3) ->
    workday;
dotw_to_daytype(christian,4) ->
    workday;
dotw_to_daytype(christian,5) ->
    workday;
dotw_to_daytype(christian,6) ->
    weekend;
dotw_to_daytype(christian,7) ->
    weekend;
dotw_to_daytype(muslim,1) ->
    workday;
dotw_to_daytype(muslim,2) ->
    workday;
dotw_to_daytype(muslim,3) ->
    workday;
dotw_to_daytype(muslim,4) ->
    weekend;
dotw_to_daytype(muslim,5) ->
    weekend;
dotw_to_daytype(muslim,6) ->
    workday;
dotw_to_daytype(muslim,7) ->
    workday.
