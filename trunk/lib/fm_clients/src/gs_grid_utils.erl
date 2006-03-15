%%%-------------------------------------------------------------------
%%% File    : gs_grid_utils.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 16 Aug 2003 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(gs_grid_utils).

-export([add_row/4,del_row/3,mod_field/4]).


%---------------------------------------------------------------
% Add a new row at position Pos.
%
add_row(Gr,Pos,Rows,Opts) ->
   LOpts=mk_line_opts(Opts),
   add_row1(Gr,Pos,Rows,LOpts).

add_row1(Gr,Pos,Rows,LOpts) when Pos=<Rows ->
   SLOpts=read_line(Gr,Pos,[text,data]),
   add_line(Gr,Pos,LOpts),
   add_row1(Gr,Pos+1,Rows,SLOpts);

add_row1(Gr,Pos,Rows,LOpts) when Pos>Rows ->
   add_line(Gr,Pos,LOpts).

mk_line_opts(Opts) ->
   lists:flatten(lists:map(fun (Opt) -> mk_line_opt(Opt) end, Opts)).

mk_line_opt({text,Cols}) ->
   mk_text_opt(Cols,1);
mk_line_opt(Opt) ->
   Opt.

mk_text_opt([Txt|Txts],Colno) ->
   [{text,{Colno,Txt}}|mk_text_opt(Txts,Colno+1)];
mk_text_opt([],_Colno) ->
   [].


%%---------------------------------------------------------------
%% Delete Row[Row]
%%
del_row(Gr,Row,Rows) when Row =< Rows->
   LOpts=read_line(Gr,Row+1,[text,data]),
   add_line(Gr,Row,LOpts),
   del_row(Gr,Row+1,Rows);

del_row(Gr,Row,Rows) ->
   Gl=gs:read(Gr,{obj_at_row,Row}),
   gs:destroy(Gl).

%%---------------------------------------------------------------
%% Modify a field value
%%
mod_field(Gr,Row,Col,Val) ->
   Gl=gs:read(Gr,{obj_at_row,Row}), 
   X=gs:read(Gl,text), 
   gs:config(Gl,[{text,{Col,Val}}]).

%%---------------------------------------------------------------
%% Add a Row[Row]
%%
add_line(Gr,Row,LOpts)->
   case gs:read(Gr,{obj_at_row,Row}) of
      undefined ->
         R=gs:gridline(Gr,[{row,Row}|LOpts]),
         R;
      GL2 ->
         gs:config(GL2,LOpts)
   end.

read_line(Gr,Row) ->
   read_line(Gr,Row,[text]).

read_line(Gr,Row,Opts) when is_list(Opts) ->
   Cols=length(gs:read(Gr,columnwidths)),
   Gl=gs:read(Gr,{obj_at_row,Row}),
   LOpts=read_opts(Gl,Cols,Opts).

read_opts(Gl,Cols,Opts) ->
   lists:flatten(lists:map(fun (Op) ->
                              read_opt(Gl,Cols,Op)
                           end, Opts)).

read_opt(Gl,Cols,text) ->
   read_text(Gl,Cols,1);

read_opt(Gl,_Cols,data) ->
   Data=gs:read(Gl,data),
   {data,Data};

read_opt(Gl,_Cols,bg) ->
   Bg=gs:read(Gl,bg),
   {bg,Bg}.

read_text(Gl,Cols,Col) when Col =< Cols ->
   Txt=case Gl of
          undefined ->
             "";
          Gl ->
              gs:read(Gl,{text,Col})
       end,
   [{text,{Col,Txt}}|read_text(Gl,Cols,Col+1)];
read_text(_Gl,_Cols,_Col) ->
   [].

