%%%-------------------------------------------------------------------
%%% File    : al_sts_client.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 13 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(al_sts_client).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
-include("CIM_AlertIndication.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {lang,ed}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Name,Lang) ->
    gen_server:start(?MODULE, [Name,Lang], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Name,Lang]) ->
    Filter=mkFilter(),
    Ed=mk_win(Name),
    AList=alarm_sts_server:subscribe(Name,Filter),
    lists:foreach(fun (A) ->
			  print(Ed,Lang,A)
		  end, AList),
    {ok, #state{lang=Lang,ed=Ed}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Alarm, State) when is_record(Alarm,'CIM_AlertIndication') ->
    print(State#state.ed,State#state.lang,Alarm),
    {noreply, State};

handle_info(Alarm, State) when is_record(Alarm,'CIM_ThresholdIndication') ->
    print(State#state.ed,State#state.lang,Alarm),
    {noreply, State};

handle_info({gs,_Id,destroy,_Data,_Arg}, State) -> 
    {stop, normal, State};

handle_info({gs,_Id,configure,_Data,[W,H|_]},State) ->
     gs:config(packer,[{width,W-5},{height,H-30}]), % repack
    {noreply, State};

handle_info({gs,_Id,click,_,["Exit"|_]},State) ->
    {stop, normal, State};
handle_info({gs,_Id,click,_,["Clear"|_]},State) ->
    clear_win(State#state.ed),
    {noreply, State};
handle_info({gs,_Id,keypress,[],['Up'|_]},State) ->
    line_up(State#state.ed),
    {noreply, State};
handle_info({gs,_Id,keypress,[],['Down'|_]},State) ->
    line_down(State#state.ed),
    {noreply, State};
handle_info({gs,_Id,keypress,[],['Next'|_]},State) ->
    page_down(State#state.ed),
    {noreply, State};
handle_info({gs,_Id,keypress,[],['Prior'|_]},State) ->
    page_up(State#state.ed),
    {noreply, State};

handle_info(Info, State) ->
    io:format("loop got: ~p~n",[Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

mkFilter() ->
    Me=self(),
    ets:fun2ms(fun (X) ->
		       {{send_to,Me},X}
	       end).
    
mk_win(Title) ->
    WH=[{width,400},{height,300}],
    BG={bg,{157,185,200}},
    FG={fg,{157,185,200}},
    Win = gs:window(gs:start(),[BG,{map,true},{configure,true},
				{keypress,true},
				{title,atom_to_list(Title)}|WH]),
    gs:frame(packer,Win,[{x, 3}, {y, 25},{packer_x,[{stretch,1,50}]},
                         {packer_y,[{stretch,1}]}]),
    Bar=gs:create(menubar,Win,[BG]),
    Fmb=gs:create(menubutton,Bar,[{label,{text,"File"}}]),
    Vmb=gs:create(menubutton,Bar,[{label,{text,"View"}}]),
    Fmnu=gs:create(menu,Fmb,[]),
    Vmnu=gs:create(menu,Vmb,[]),
    gs:create(menuitem,Fmnu,[{label,{text,"Exit"}}]),
    gs:create(menuitem,Vmnu,[{label,{text,"Clear"}}]),
%    gs:button(packer,[{label,{text,"Close"}},{pack_xy,{2,2}}]),
    Ed=gs:editor(packer,[{bg,{195, 195, 195}},{pack_xy,{1,1}},
			 {scrollbg,{209, 228, 238}},{vscroll,right},%{hscroll,true},
			 {wrap,word},{enable,false}]),
    gs:config(packer,WH), % refresh to initial size
    Ed.

clear_win(Ed) ->
    gs:config(Ed,{enable,true}),
    gs:config(Ed,clear),
    gs:config(Ed,{enable,false}).
    
line_down(Ed) ->
    gs:config(Ed,{enable,true}),
    NoRows=gs:read(Ed,size),
    RPS=gs:read(Ed,char_height),
    case gs:read(Ed,vscrollpos) of
	N when N<NoRows  ->
	    gs:config(Ed,{vscrollpos,N+1});
	_ ->
	    ok
    end,
    gs:config(Ed,{enable,false}).

line_up(Ed) ->
    gs:config(Ed,{enable,true}),
    case gs:read(Ed,vscrollpos) of
	1 ->
	    ok;
	N ->
	    gs:config(Ed,{vscrollpos,N-1})
    end,
    gs:config(Ed,{enable,false}).

page_down(Ed) ->
    ok.
    
page_up(Ed) ->
    ok.
    
print(Ed,Lang,Alarm) ->
    Lines=lists:reverse(format_alarm(Alarm,Lang)),
    gs:config(Ed,{enable,true}),
    Hd=io_lib:format("=== New Alarm =============================~n~n",[]),
    Tl=io_lib:format("~n~n",[]),
    gs:config(Ed,{insert,{'end',Hd}}),
    lists:foreach(fun (L) ->
			  gs:config(Ed,{insert,{'end',L}})
		  end, Lines),
    gs:config(Ed,{insert,{'end',Tl}}),
    gs:config(Ed,{enable,false}).


format_alarm(Alarm,Lang) when is_record(Alarm,'CIM_AlertIndication') ->
    AList=tl(tuple_to_list(Alarm)),
    format('CIM_AlertIndication',AList,Lang);
format_alarm(Alarm,Lang)  when is_record(Alarm,'CIM_ThresholdIndication') ->
    L1=format_alarm(Alarm#'CIM_ThresholdIndication'.'CIM_AI',Lang),
    TI=tl(tl(tuple_to_list(Alarm))),
    format('CIM_ThresholdIndication',TI,Lang,1,L1).

format(Type,List,Lang) ->
    format(Type,List,Lang,1,[]).

format(Type,[undefined|As],Lang,N,L) ->
    format(Type,As,Lang,N+1,L);
format(Type,[A|As],Lang,N,L) ->
    format(Type,As,Lang,N+1,[format(Type,N,Lang,A)|L]);
format(Type,[],Lang,N,L) ->
    L.

format(Type,?INDICATIONTIME=Key,Lang,{{YY,MM,DD},{HH,MIN,SS}}) ->
    case nls:getString(Type,Key,Lang) of
	{found,String} ->
	    io_lib:format("~s: ~w/~w/~w ~w:~w:~w~n",[String,YY,MM,DD,HH,MIN,SS]);
	_ ->
	    io_lib:format("UNDEFINED ~w/~w/~w~n",[Type,Key,Lang])
    end;

format(Type,Key,Lang,Val) ->
    CVal=convert_val(Type,Key,Lang,Val),
    case nls:getString(Type,Key,Lang) of
	{found,String} ->
	    io_lib:format("~s: ~s~n",[String,CVal]);
	_ ->
	    io_lib:format("UNDEFINED ~w/~w/~w~n",[Type,Key,Lang])
    end.

convert_val('CIM_AlertIndication'=Type,?ALERTTYPE=Key,Lang,Val) ->
    getString(alert_type,Val,Lang);
convert_val('CIM_AlertIndication'=Type,?PERCEIVEDSEVERITY=Key,Lang,Val) ->
    getString(perceived_severity,Val,Lang);
convert_val('CIM_AlertIndication'=Type,?PROBABLECAUSE=Key,Lang,Val) ->
    getString(probable_cause,Val,Lang);
convert_val('CIM_AlertIndication'=Type,?TRENDING=Key,Lang,Val) ->
    getString(trending,Val,Lang);
convert_val(Type,Key,Lang,Val) when is_integer(Val) ->
    integer_to_list(Val);
convert_val(Type,Key,Lang,Val) when is_atom(Val) ->
    atom_to_list(Val);
convert_val(Type,Key,Lang,Val) ->
    Val.

getString(Type,Val,Lang) ->
    case nls:getString(Type,Val,Lang) of
	{found,String} ->
	    String;
	_ ->
	    integer_to_list(Val)
    end.
