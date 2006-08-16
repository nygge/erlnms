%%%-------------------------------------------------------------------
%%% @copyright 2006 Anders Nygren
%%% File    : yaws_pm_basic.erl
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc 
%%% @end 
%%% Created : 13 Aug 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(yaws_pm_basic).

-import(yaws_api,[f/2]).

-export([aggregate_list/0,
	 aggregate_view/1,
	 archive_list/0,
	 archive_view/1,
	 counter_list/0,
	 counter_view/2,
	 der_counter_list/0,
	 duration_list/0,
	 duration_view/1,
	 event_list/0,
	 mo_type_list/0,
	 mo_type_view/1,
	 store_type_list/0,
	 store_type_view/1
	]).
-include("pm_store.hrl").

aggregate_list() ->
    aggregate_list(pm_basic:get_all_aggregates()).

aggregate_list(As) ->
    {table,[],
     [{caption,[],"Aggregates"},
      {tr,[],
       [{th,[],Heading}||Heading <- ["Name","CF","XFF","Resolution",
				     "Duration","Edit","Delete"]]
      }|
      lists:map(
	fun (A) ->
		{tr,[],
		 [{td,[],A#pm_aggregate.name},
		  {td,[],f("~s",[A#pm_aggregate.cf])},
		  {td,[],f("~p",[A#pm_aggregate.xff])},
		  {td,[],{a,[{href,f("../duration/view.yaws?name=~s",
				     [A#pm_aggregate.resolution])}],
			      A#pm_aggregate.resolution}},
		  {td,[],{a,[{href,f("../duration/view.yaws?name=~s",
				     [A#pm_aggregate.duration])}],
			      A#pm_aggregate.duration}},
		  {td,[],{a,[{href,f("edit.yaws?name=~s",
				     [A#pm_aggregate.name])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?name=~s",
				     [A#pm_aggregate.name])}],"Delete"}}]}
	end,lists:keysort(2,As))]}.

aggregate_view(Name) ->
    A=pm_basic:get_aggregate(Name),
    io:format("~p,~p~n",[Name,A]),
    {table,[],
     [{caption,[],"Aggregate"},
      {tr,[],
       [{th,[],"Name"},{td,[],A#pm_aggregate.name}]},
      {tr,[],
       [{th,[],"CF"},{td,[],f("~s",[A#pm_aggregate.cf])}]},
      {tr,[],
       [{th,[],"XFF"},{td,[],f("~p",[A#pm_aggregate.xff])}]},
      {tr,[],
	[{th,[],"Resolution"},
	 {td,[],{a,[{href,f("../duration/view.yaws?name=~s",
			    [A#pm_aggregate.resolution])}],
		 A#pm_aggregate.resolution}}]},

      {tr,[],
       [{th,[],"Duration"},
	{td,[],{a,[{href,f("../duration/view.yaws?name=~s",
			   [A#pm_aggregate.duration])}],
		A#pm_aggregate.duration}}
       ]}]}.
    
archive_list() ->
    As=lists:keysort(2,pm_basic:get_all_archives()),
    {table,[],
     [{caption,[],"Archives"},
      {tr,[],
       [{th,[],Heading}||Heading <- ["Name","Edit","Delete"]]
      }|
      lists:map(
	fun (A) ->
		{tr,[],
		 [{td,[],{a,[{href,f("view.yaws?name=~s",
				     [A#pm_archive.name])}],
			  A#pm_archive.name}},

		  {td,[],{a,[{href,f("edit.yaws?name=~s",
				     [A#pm_archive.name])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?name=~s",
				     [A#pm_archive.name])}],"Delete"}}]}
	end,As)]}.

archive_view(Name) ->
    Ar=pm_basic:get_archive(Name),
    Name=Ar#pm_archive.name,
    [{h2,[],Name},
     aggregate_list([pm_basic:get_aggregate(A) || A <-Ar#pm_archive.aggregates])
    ].

counter_list() ->
    Cs=lists:keysort(2,pm_basic:get_all_counters()),
    {table,[],
     [{caption,[],"Counters"},
      {tr,[],
       [{th,[],Heading}||
	   Heading <- 
	       ["MO Type","Name","Type","Heartbeat","Min","Max","Edit","Delete"]]
      }|
      lists:map(
	fun (C) ->
		#pm_counter{name={MOName,CName},type=Type,hb=HB,
			    min=Min,max=Max}=C,
		
		{tr,[],
		 [{td,[],{a,[{href,f("../mo_type/view.yaws?name=~s",
				     [MOName])}],MOName}},
		  {td,[],CName},
		  {td,[],f("~s",[Type])},
		  {td,[],{a,[{href,f("../duration/view.yaws?name=~s",
				     [HB])}],HB}},
		  {td,[{align,right}],f("~p",[Min])},
		  {td,[{align,right}],f("~p",[Max])},
		  {td,[],{a,[{href,f("edit.yaws?mo_name=~s&cname=~s",
				     [MOName,CName])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?mo_name=~s&cname=~s",
				     [MOName,CName])}],"Delete"}}]}
	end,Cs)]}.
    
counter_view(MOType,Name) ->
    #pm_counter{name={MOName,CName},type=Type,hb=HB,
		  min=Min,max=Max}=pm_basic:get_counter({MOType,Name}),
    {table,[],
     [{caption,[],"Counter"},
      {tr,[],
       [{th,[],Heading}||
	   Heading <- 
	       ["MO Type","Name","Type","Heartbeat","Min","Max","Edit","Delete"]]
      },
      {tr,[],
       [{td,[],{a,[{href,f("../mo_type/view.yaws?name=~s",
			   [MOName])}],MOName}},
	{td,[],CName},
	{td,[],f("~s",[Type])},
	{td,[],{a,[{href,f("../duration/view.yaws?name=~s",[HB])}],HB}},
	{td,[{align,right}],f("~p",[Min])},
	{td,[{align,right}],f("~p",[Max])},
	{td,[],{a,[{href,f("edit.yaws?mo_name=~s&cname=~s",
			   [MOName,CName])}],"Edit"}},
	{td,[],{a,[{href,f("delete.yaws?mo_name=~s&cname=~s",
			   [MOName,CName])}],"Delete"}}]}]}.

der_counter_list() ->
    Cs=lists:keysort(2,pm_basic:get_all_der_counters()),
    {table,[],
     [{caption,[],"Derived Counters"},
      {tr,[],
       [{th,[],Heading}||
	   Heading <- 
	       ["MO Type","Name","Expression","Dependencies","Edit","Delete"]]
      }|
      lists:map(
	fun (C) ->
		#pm_der_counter{name={MOName,CName},expr=Expr,
				deps=Deps}=C,
		
		{tr,[],
		 [{td,[],{a,[{href,f("../mo_type/view.yaws?name=~s",
				     [MOName])}],MOName}},
		  {td,[],CName},
		  {td,[],f("~s",[Expr])},
		  {td,[],f("~p",[Deps])},
		  {td,[],{a,[{href,f("edit.yaws?mo_name=~s&cname=~s",
				     [MOName,CName])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?mo_name=~s&cname=~s",
				     [MOName,CName])}],"Delete"}}]}
	end,Cs)]}.

duration_list() ->
    Ds=lists:keysort(2,pm_basic:get_all_durations()),
    {table,[],
     [{caption,[],"Durations"},
      {tr,[],
       [{th,[],Heading}||
	   Heading <- 
	       ["Name","Value","Unit","Edit","Delete"]]
      }|
      lists:map(
	fun (#pm_duration{name=Name,value={Unit,Val}}) ->
		{tr,[],
		 [{td,[],Name},
		  {td,[{align,right}],f("~p",[Val])},
		  {td,[],f("~s",[Unit])},
		  {td,[],{a,[{href,f("edit.yaws?name=~s",
				     [Name])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?name=~s",
				     [Name])}],"Delete"}}]}
	end,Ds)]}.


duration_view(Name) ->
    #pm_duration{name=Name,value={Unit,Val}}=pm_basic:get_duration(Name),

    {table,[],
     [{caption,[],"Duration"},
      {tr,[],
       [{th,[],"Name"},{td,[],Name}]},
      {tr,[],
       [{th,[],"Unit"},{td,[],f("~s",[Unit])}]},
      {tr,[],
       [{th,[],"Value"},{td,[],f("~p",[Val])}]}
     ]}.

event_list() ->
    EVs=pm_basic:get_all_events(),
    {table,[],
     [{caption,[],"Events"},
      {tr,[],
       [{th,[],Heading}||Heading <- ["MO Instance","Events",
				     "Edit","Delete"]]
      }|
      lists:map(
	fun (#pm_event{id=Name,events=Events}) ->
		{tr,[],
		 [{td,[],{a,[{href,f("view.yaws?name=~p",
				     [Name])}],f("~p",[Name])}},
		  {td,[],f("~p",[Events])},

		  {td,[],{a,[{href,f("edit.yaws?name=~p",
				     [Name])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?name=~p",
				     [Name])}],"Delete"}}]}
	end,EVs)]}.

mo_type_list() ->
    MOs=pm_basic:get_all_mo_types(),
    {table,[],
     [{caption,[],"MO Types"},
      {tr,[],
       [{th,[],Heading}||Heading <- ["Name","Edit","Delete"]]
      }|
      lists:map(
	fun (#pm_mo_type{name=Name}) ->
		{tr,[],
		 [{td,[],{a,[{href,f("view.yaws?name=~s",
				     [Name])}],Name}},

		  {td,[],{a,[{href,f("edit.yaws?name=~s",
				     [Name])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?name=~s",
				     [Name])}],"Delete"}}]}
	end,MOs)]}.

mo_type_view(Name) ->
    #pm_mo_type{name=Name, counters=Cs, der_counters=DCs}=
	pm_basic:get_mo_type(Name),
    TCs=[{h2,[],["MO Type : ",Name]},
	 {table,[],
	  [{caption,[],"Counters"},
	   {tr,[],
	    [{th,[],Heading}||
		Heading <- ["Name","Type","Heartbeat","Min","Max","Remove"]]
	   }|
	   lists:map(
	     fun (C) ->
		     #pm_counter{name={Name,C},type=Type,hb=HB,
				 min=Min,max=Max}=
			 pm_basic:get_counter({Name,C}),
		     {tr,[],
		      [
		       {td,[],{a,
			       [{href,
				 f("../counter/view.yaws?mo_type=~s&name=~s",
				   [Name,C])}],C}},
		       {td,[],f("~s",[Type])},
		       {td,[],{a,[{href,f("../duration/view.yaws?name=~s",
					  [HB])}],HB}},
		       {td,[{align,right}],f("~p",[Min])},
		       {td,[{align,right}],f("~p",[Max])},
		       {td,[],{a,[{href,f("remove.yaws?name=~s,counter=~s",
					  [Name,C])}],"Remove"}}]}
	     end,Cs)]}],
    TDCs= [{table,[],
	    [{caption,[],"Derived Counters"},
	     {tr,[],
	      [{th,[],Heading}||
		  Heading <- 
		      ["Name","Expression",
		       "Dependencies","Edit","Delete"]]
	     }|
	     lists:map(
	       fun (C) ->
		       #pm_der_counter{name={MOName,CName},expr=Expr,
				       deps=Deps}=
			   pm_basic:get_der_counter({Name,C}),

		       {tr,[],
			[
			 {td,[],{a,
				 [{href,
				   f("../der_counter/view.yaws?mo_type=~s&name=~s",
				     [Name,C])}],C}},
			 {td,[],f("~s",[Expr])},
			 {td,[],f("~p",[Deps])},
			 {td,[],{a,[{href,f("remove.yaws?name=~s,counter=~s",
					    [Name,C])}],"Remove"}}]}
	       end,DCs)]}],
    [TCs,TDCs].


store_type_list() ->
    STs=pm_basic:get_all_store_types(),
    {table,[],
     [{caption,[],"Storage Types"},
      {tr,[],
       [{th,[],Heading}||Heading <- ["Name","MO Type","Archive","Step",
				     "Edit","Delete"]]
      }|
      lists:map(
	fun (#pm_store_type{name=Name,mo_type=MOType,archive=Arch,step=Step}) ->
		{tr,[],
		 [{td,[],{a,[{href,f("view.yaws?name=~s",
				     [Name])}],Name}},

		  {td,[],{a,[{href,f("../mo_type/view.yaws?name=~s",
				     [MOType])}],MOType}},

		  {td,[],{a,[{href,f("../archive/view.yaws?name=~s",
				     [Arch])}],Arch}},
		  
		  {td,[],{a,[{href,f("../duration/view.yaws?name=~s",
				     [Step])}],Step}},
		  {td,[],{a,[{href,f("edit.yaws?name=~s",
				     [Name])}],"Edit"}},
		  {td,[],{a,[{href,f("delete.yaws?name=~s",
				     [Name])}],"Delete"}}]}
	end,STs)]}.

store_type_view(Name) ->
    #pm_store_type{name=Name,mo_type=MOType,archive=Arch,step=Step}=
	pm_basic:get_store_type(Name),
    [mo_type_view(MOType),archive_view(Arch)].
