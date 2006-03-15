%%%-------------------------------------------------------------------
%%% File    : alarm_list.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 16 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(alarm_list).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("stdlib/include/ms_transform.hrl").
-include("CIM_AlertIndication.hrl").
-include("alarm_sts.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start/2,ack_to_list/1,get_attr/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
	 code_change/3]).

-record(state, {user=anders,
		lang,
		e_mgr,
		grid,
		ed,
		a_list=[],
		al_size,
		selected=none}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/2
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
    case alarm_sts_server:subscribe(Name,Filter) of
	{Pid,AL} when is_pid(Pid) ->
	    {Gr,Ed}=mk_win(Name),
	    AList=lists:keysort(2,AL),
	    Size=length(AList),
	    load_alarm_list(Gr,AList,Lang),
	    {ok, #state{lang=Lang,e_mgr=Pid,grid=Gr,ed=Ed,
			a_list=AList,al_size=Size}};
	{error,Reason} ->
	    {stop,Reason}
    end.

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
handle_info(Alarm, State) when is_record(Alarm,as_alarm) ->
    Id=Alarm#as_alarm.id,
    AL=State#state.a_list,
    case lists:keysearch(Id,2,AL) of
	{value,X} ->
	    {value,Pos}=lists2:find_key_pos(Id, 2, AL),
	    NAList=lists:keyreplace(Id,2,AL,Alarm),
	    NState=State#state{a_list=NAList},
	    update_alarm(Alarm,Pos,
			 NState),
	    {noreply, NState};
	false ->
	    AList=lists:keysort(2,[Alarm|AL]),
	    %io:format("ALarmList=~p~n",[AList]),
	    {value,Pos}=lists2:find_key_pos(Id, 2, AList),
	    add_alarm(Alarm, State#state.grid, State#state.lang, Pos,
		      State#state.al_size),
	    Size=State#state.al_size+1,
	    {noreply, State#state{a_list=AList,al_size=Size}}
    end;

handle_info(A, State) when is_record(A,as_ack) ->
    Id=A#as_ack.id,
    AL=State#state.a_list,
    case lists:keysearch(Id,2,AL) of
	{value,Alarm} ->
	    {value,Pos}=lists2:find_key_pos(Id, 2, AL),
	    update_alarm(Alarm#as_alarm{ack=#ack{who=A#as_ack.who,
						 time=A#as_ack.time}},
			 Pos,
			 State),
	    NAList=lists:keyreplace(Id,2,AL,Alarm),
	    {noreply, State#state{a_list=NAList}};
	_ ->
	    {noreply,State}
    end;

handle_info(A, State) when is_record(A,as_cease) ->
    Id=A#as_cease.id,
    AL=State#state.a_list,
    case lists2:find_key_pos(Id, 2, AL) of
        {value,Pos} ->
	    NState=del_alarm(Id,Pos,State),
	    NAList=lists:keydelete(Id,2,AL),
	    {noreply, NState#state{a_list=NAList}};
	_ ->
	    {noreply,State}
    end;

handle_info(A, State) when is_record(A,as_clear) ->
    Id=A#as_clear.id,
    AL=State#state.a_list,
    case lists2:find_key_pos(Id, 2, AL) of
        {value,Pos} ->
	    {value,Pos}=lists2:find_key_pos(Id, 2, AL),
	    NState=del_alarm(Id,Pos,State),
	    NAList=lists:keydelete(Id,2,AL),
	    {noreply, NState#state{a_list=NAList}};
	_ ->
	    {noreply,State}
    end;

handle_info({gs,_Id,destroy,_Data,_Arg}, State) -> 
    {stop, normal, State};
handle_info({gs,Id,click,_,["Exit"|_]},State) ->
    {stop,normal,State};
handle_info({gs,_,_,_,_}=Event,State)->
    N_State=gs_event(Event,State),
    {noreply,N_State};

handle_info(Info, State) ->
%%    io:format("alarm_list:handle_info => ~w~n",[Info]),
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
    
mk_win(ME) ->
    WH=[{width,700},{height,700}],
    BG={bg,{157,185,200}},
    FG={fg,{157,185,200}},
    Title=io_lib:format("~w",[ME]),
    Win = gs:window(gs:start(),[BG,{map,true},{configure,true},
				{keypress,true},
				{title,"Alarm List: "++Title}|WH]),
    gs:frame(packer,Win,[{x, 3}, {y, 25},
			 {packer_x,[{stretch,1,50},
				    {stretch,1,50},
				    {stretch,1,50}]},
                         {packer_y,[{stretch,1},
				    {fixed,30},
				    {stretch,1,50,200}]}]),
    Bar=gs:create(menubar,Win,[BG]),
    Fmb=gs:create(menubutton,Bar,[{label,{text,"File"}}]),
    Fmnu=gs:create(menu,Fmb,[]),
    gs:create(menuitem,Fmnu,[{label,{text,"Exit"}}]),
    Gr=gs:grid(packer,[{columnwidths,[40,80,80,200,200,100]},{pack_xy,{{1,3},1}}]),
    gs:button(ack,packer,[{label,{text,"Ack"}},{pack_xy,{1,2}}]),
    gs:button(clear,packer,[{label,{text,"Clear"}},{pack_xy,{2,2}}]),
    gs:button(comment,packer,[{label,{text,"Comment"}},{pack_xy,{3,2}}]),
    Ed=gs:editor(packer,[{bg,{195, 195, 195}},
			 {scrollbg,{209, 228, 238}},
			 {vscroll,right}, {hscroll,true},
			 {pack_xy,{{1,3},3}},
			 {wrap,word},{enable,false}]),
    gs:config(packer,WH), % refresh to initial size
    {Gr,Ed}.

load_alarm_list(Gr,AList,Lang) ->
    As=lists:map(fun(A) ->
			 mkLOpts(A,Lang)
		 end, AList),
    init_rows(Gr,As).

add_alarm(Alarm,Gr,Lang,Pos,Rows) ->
    LOpts=mkLOpts(Alarm,Lang),
    add_row(Gr,Pos,Rows,LOpts).

mkLOpts(#as_alarm{id=Id,ack=Ack,alarm=A,comments=_},Lang) ->
    [{data,Id},
     {text,[ack_to_list(Ack),
	    get_attr(A,'AlertingManagedElement',Lang),
	    get_attr(A,'PerceivedSeverity',Lang),
	    get_attr(A,'AlertType',Lang),
	    get_attr(A,'ProbableCause',Lang),
	    get_attr(A,'Trending',Lang)]}].
    
ack_to_list(nack) ->
    "Nack";
ack_to_list(A) when is_record(A,ack) ->
    "Ack".

get_attr(A,Attr,Lang) when is_record(A, 'CIM_ThresholdIndication') ->
    get_attr(A#'CIM_ThresholdIndication'.'CIM_AI',Attr,Lang);
get_attr(A,'AlertingManagedElement',Lang) when is_record(A, 'CIM_AlertIndication') ->
    ME=getME(A#'CIM_AlertIndication'.'AlertingManagedElement'),
    io_lib:format("~p",[ME]);
get_attr(A,'AlertType',Lang) when is_record(A, 'CIM_AlertIndication') ->
    getString(alert_type,A#'CIM_AlertIndication'.'AlertType',Lang);
get_attr(A,'PerceivedSeverity',Lang)  when is_record(A, 'CIM_AlertIndication') ->
    getString(perceived_severity,
	      A#'CIM_AlertIndication'.'PerceivedSeverity',Lang);
get_attr(A,'ProbableCause',Lang)  when is_record(A, 'CIM_AlertIndication') ->
    getString(probable_cause,
	      A#'CIM_AlertIndication'.'ProbableCause',Lang);
get_attr(A,'Trending',Lang)  when is_record(A, 'CIM_AlertIndication') ->
    getString(trending,
	      A#'CIM_AlertIndication'.'Trending',Lang);
get_attr(A,'EventTime',Lang)  when is_record(A, 'CIM_AlertIndication') ->
    convert_val('CIM_AlertIndication',?EVENTTIME,Lang,
		A#'CIM_AlertIndication'.'EventTime').

getME([ME]) ->
    ME;
getME([ME|More]) ->
    More.

%%---------------------------------------------------------------
%% Add all rows at init
%%
init_rows(Gr,Rows) ->
    init_rows(Gr,Rows,1).

init_rows(Gr,[Row|Rows],Pos) ->
    add_row(Gr,Pos,Pos,Row),
    init_rows(Gr,Rows,Pos+1);

init_rows(_Gr,[],_Pos) ->
    [].

%%---------------------------------------------------------------
%% Add a new row at position Pos.
%%
add_row(Gr,Pos,Rows,LOpts) ->
    gs_grid_utils:add_row(Gr,Pos,Rows,LOpts).

%%---------------------------------------------------------------
%% Update the alarm at position Pos.
%%
update_alarm(Alarm,Pos,State) ->
    LOpts=mkLOpts(Alarm,State#state.lang),
    Rows=State#state.al_size,
    Gr=State#state.grid,
    del_row(Gr,Pos,Rows),
    add_row(Gr,Pos,Rows-1,LOpts),
    case State#state.selected of
	Pos ->
	    show_alarm(Pos,State);
	Other ->
	    ok
    end.
    
%%---------------------------------------------------------------
%% Delete the alarm at position Pos.
%%
del_alarm(Id,Pos,State) ->
    Ed=State#state.ed,
    NSel=case State#state.selected of
	     Pos ->
		 gs:config(Ed,{enable,true}),
		 gs:config(Ed,clear),
		 gs:config(Ed,{enable,false}),
		 deselect(State,Pos);
	     Other ->
		 Other
	 end,
    del_row(State#state.grid,
 	    Pos,
 	    State#state.al_size),
    NSize=State#state.al_size-1,
    NState=State#state{selected=NSel,al_size=NSize}.
	    
%%---------------------------------------------------------------
%% Delete the row at position Pos.
%%
del_row(Gr,Pos,Rows) ->
    gs_grid_utils:del_row(Gr,Pos,Rows),
    ok=gs:config(Gr,{rows,{1,Rows-1}}).

%%---------------------------------------------------------------
%% Change the status of the row at position Pos.
%%
%%chg_sts(Gr,Row,STS) ->
%%    gs_grid_utils:mod_field(Gr,Row,2,atom_to_list(STS)).


show_alarm(Row,State) when is_integer(Row) ->
    GL=gs:read(State#state.grid,{obj_at_row,Row}),
    show_alarm(GL,State);

show_alarm(Gridline,State) ->
    Id=gs:read(Gridline,data),
    {value,Alarm}=lists:keysearch(Id,2,State#state.a_list),
    Ed=State#state.ed,
    Lang=State#state.lang,
    gs:config(Ed,{enable,true}),
    gs:config(Ed,clear),
    alarm_trace:print(Ed,Lang,Alarm),
    gs:config(Ed,{enable,false}).
   

convert_val('CIM_AlertIndication'=Type,?ALERTTYPE=Key,Lang,Val) ->
    getString(alert_type,Val,Lang);
convert_val('CIM_AlertIndication'=Type,?PERCEIVEDSEVERITY=Key,Lang,Val) ->
    getString(perceived_severity,Val,Lang);
convert_val('CIM_AlertIndication'=Type,?PROBABLECAUSE=Key,Lang,Val) ->
    getString(probable_cause,Val,Lang);
convert_val('CIM_AlertIndication'=Type,?TRENDING=Key,Lang,Val) ->
    getString(trending,Val,Lang);
convert_val('CIM_AlertIndication',?EVENTTIME,Lang,{{YY,MM,DD},{HH,MIN,SS}}) ->
    io_lib:format("~w/~w/~w ~w:~w:~w~n",
				  [YY,MM,DD,HH,MIN,SS]);
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
    
gs_event({gs,_Id,configure,_Data,[W,H|_]}=E,State) ->
    gs:config(packer,[{width,W},{height,H}]),
    State;
gs_event({gs,ack,click,_Data,_},State) ->
    case selected(State) of
	none ->
	    ok;
	Id ->
	    send(ack,Id,State#state.e_mgr,State#state.user)
    end,
    State;
gs_event({gs,clear,click,Data,_},State) ->
    case selected(State) of
 	none ->
 	    ok;
 	Id ->
	    
 	    send(clear,Id,State#state.e_mgr,State#state.user)
    end,
     State;
% gs_event({gs,delete,click,_Data,_},State) ->
%     ID=selected(State),
%     send_del(ID),
%     State;

gs_event({gs,Gridline,click,Data,[Col,Row|_]},State) ->
    %%    io:format("Click at row: ~w~n",[Row]),
    ID=selected(State),
    case State#state.selected of
	none ->
	    gs:config(Gridline,{bg,white}),
	    show_alarm(Gridline,State),
	    State#state{selected=Row};
	Row ->
	    gs:config(Gridline,{bg,gray}),
	    State#state{selected=none};
	Other ->
	    Gr=State#state.grid,
	    Gl=gs:read(Gr,{obj_at_row,Other}), 
	    gs:config(Gl,{bg,gray}),
	    gs:config(Gridline,{bg,white}),
	    show_alarm(Gridline,State),
	    State#state{selected=Row}
    end;
gs_event(Event,State) ->
    State.

selected(State) ->
    case State#state.selected of
	none ->
	    none;
	Row ->
	    Gl=gs:read(State#state.grid,{obj_at_row,Row}),
	    ID=gs:read(Gl,data)
    end.

deselect(State,Row) when State#state.selected==Row ->
    Gl=gs:read(State#state.grid,{obj_at_row,Row}), 
    gs:config(Gl,{bg,gray}),
    State#state{selected=none};
deselect(State,Row) ->
    State.

%%========================================================================
%%
%%    send/3,4
%%    Send message to alarm_sts
%%

send(ack,Id,EMgr,Who) ->
    alarm_sts:ack(EMgr,Id,Who);
send(clear,Id,EMgr,Who) ->
    alarm_sts:clear(EMgr,Id,Who).

send(comment,Id,EMgr,Comment,Who) ->
    alarm_sts:comment(EMgr,Id,Comment,Who).

    
