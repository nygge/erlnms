%%%-------------------------------------------------------------------
%%% File    : rrd_lib_test.erl
%%% Author  : Anders Nygren <anders@local>
%%% Description : 
%%%
%%% Created : 26 Aug 2003 by Anders Nygren <anders@local>
%%%-------------------------------------------------------------------
-module(rrd_lib_test).

-export([create/2,update/0,graph1/0,graph/0,start_all/3,start_all/4,stop_all/2,
	 mn_insert/4,create_all/4]).

% create() ->
%     File="/home/anders/erlang/miniNMS/rrdtool/priv/ne1-ifn1.rrd",
%     DSs=[{"in",'GAUGE',10,0,3},
% 	 {"out",'GAUGE',10,0,2},
% 	 {"errorin",'GAUGE',10,0,2},
% 	 {"errorout",'GAUGE',10,0,2}
% 	],
%     RRAs=[{'LAST',0.5,1,60},    % 5 secs -> 5 min
% 	  {'LAST',0.5,12,60},   % 12 dp -> 1 min, -> 60 min
% 	  {'LAST',0.5,60,288},  % 60 dp -> 5 min, -> 24 hours
% 	  {'LAST',0.5,360,48*7} % 360 dp -> 30 min, -> 7 days
% 	 ],
%     rrdtool:create(File,DSs,RRAs,def,5).

create(N,I) ->
    SampleInt={sec,15},
    Opts=[{step,SampleInt}],
    File="/home/anders/src/erlang/miniNMS/rrd_lib/priv/NE"++integer_to_list(N)++
	"-IFN"++integer_to_list(I)++".rrd",
    DSs=[["in",'GAUGE',{sec,30},0,3],
	 ["out",'GAUGE',{sec,30},0,2],
	 ["errorin",'GAUGE',{sec,30},0,2],
	 ["errorout",'GAUGE',{sec,30},0,2]
	],
    RRAs=[{'LAST',0.5,{sec,15},{min,5}},
	  {'LAST',0.5,{min,5},{hour,6}},
	  {'LAST',0.5,{min,30},{day,1}},
	  {'LAST',0.5,{hour,1},{week,2}}
	 ],
    {ok,Port}=rrd_lib:open(),
    rrd_lib:create(Port,File,Opts,DSs,RRAs),
    rrd_lib:close(Port).

update() ->
    File="/home/anders/src/erlang/miniNMS/rrd_lib/priv/test.rrd",
    Templ=["temp"],
    Vals=[{n,[12]}],
    {ok,Port}=rrd_lib:open(),
    rrdtool:update(Port,File,Templ,Vals),
    rrd_lib:close(Port).
 
graph() ->
    File="/home/anders/src/erlang/miniNMS/rrdtool/priv/test.png",
    Flags={flags,all_flags()},
    DEF={def,[{vname1,rrd1,ds_name1,cf1},
	     {vname2,rrd2,ds_name2,cf2}]},
    CDEF={cdef,[{vname1,expr1},{vname2,expr2}]},
    PRINT={print,[{vname,cf,format}]},
    COMMENT={comment,[comment]},
    HRULE={hrule,[color,legend]},
    VRULE={vrule,[color,legend]},
    LINE={line,[{1,vname,color,legend},{2,vname,color,legend}]},
    AREA={area,[]},
    STACK={stack,[]},
    Pars=[Flags,DEF,CDEF,PRINT,COMMENT,HRULE,VRULE],
    rrdtool:graph(File,Pars).
graph1()->
    rrdtool:graph("/home/anders/src/erlang/miniNMS/rrdtool/priv/test1.gif",
		  [{flags,[{start,1062048660}]},
		   {def,[{in1,"/home/anders/src/erlang/miniNMS/rrdtool/priv/ne1-ifn1.rrd",in,'AVERAGE'}, 
			 {out1,"/home/anders/src/erlang/miniNMS/rrdtool/priv/ne1-ifn1.rrd",out,'AVERAGE'}]},
		   {line,[{1,"out1#00FF00"},
			  {1,"in1#FF0000"}]}]).
all_flags()->
    [{start,45},
     {'end',76},
     {x_grid,["MINUTE",10,"HOUR",1,"HOUR",1,0,"%X"]},
     {y_grid,[10,50]},
     alt_y_grid,
     alt_y_mrtg,
     alt_autoscale,
     alt_autoscale_max,
     {units_exponent,3},
     {vertical_label,"Label"},
     {width,10},
     {height,10},
     interlaced,
     {imginfo,"formatstring"},
     {imgformat,png},
     {background,"background"},
     {overlay,25},
     {unit,10},
     lazy,
     {upper_limit,1000},
     {lower_limit,0},
     rigid,
     {base,1024},
     logarithmic,
     {color,"Color"},
     no_legend,
     {title,"Title"},
     {step,15}].

start_all(N,M,Tick) ->
    lists:foreach(fun ({NE,IF}) ->
			  catch pm_source:start(list_to_atom("NE-"++integer_to_list(NE)++
							     "-"++integer_to_list(IF)),
						list_to_atom(integer_to_list(NE)),
						IF,
						Tick)
		  end,
		  [{X,Y} || X<-lists:seq(1,N),
			    Y<-lists:seq(1,M)]).

start_all(N,M,Tick,Ticks) ->
    lists:foreach(fun ({NE,IF}) ->
			  catch pm_source:start(list_to_atom("NE-"++integer_to_list(NE)++
							     "-"++integer_to_list(IF)),
						list_to_atom(integer_to_list(NE)),
						IF,
						Tick,Ticks)
		  end,
		  [{X,Y} || X<-lists:seq(1,N),
			    Y<-lists:seq(1,M)]).
stop_all(N,I) ->
    lists:foreach(fun({NE,IF})->
			  catch pm_source:stop(list_to_atom("NE-"++integer_to_list(NE)++
						      "-"++integer_to_list(IF)))
		  end,
		  [{X,Y}||X<-lists:seq(1,N),
			  Y<-lists:seq(1,I)]).
    
create_all(N1,N2,I1,I2) ->
    lists:foreach(fun({X,Y})->
			  create(X,Y)
		  end,
		  [{X,Y}||X<-lists:seq(N1,N2),
			  Y<-lists:seq(I1,I2)]).

mn_insert(N1,N2,I1,I2) ->
    lists:foreach(fun({NE,IF})->
			  File="/home/anders/src/erlang/miniNMS/rrdtool/priv/NE"++
			      integer_to_list(NE)++
			      "-IFN"++integer_to_list(IF)++".rrd",
			  ME=[{ne,list_to_atom(integer_to_list(NE))},
			      {ifn,IF}],
			  mnesia:dirty_write(pm_store,{pm_store,
						       ME,
						       File,
						       [in,out,errorin,errorout]})
		  end,
		  [{X,Y}||X<-lists:seq(N1,N2),
			  Y<-lists:seq(I1,I2)]).
