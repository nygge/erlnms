%%%-------------------------------------------------------------------
%%% File    : rel.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created : 12 Mar 2006 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------
-module(rel).

-export([read/1]).

read(File) ->
    {ok,[{Name,Vsn,{erts,Erts},{apps,Apps},
	  {nodes,Nodes},{dist,Dist}}]}=file:consult(File),
    Rels=[mk_rel(Name,Vsn,Erts,Node,Apps) || Node <- Nodes],
    Dists=mk_dists(Nodes,Dist),
    NNodes=ins_dists(Nodes,Dists),
    Configs=[mk_config(Node,Apps) || Node <- NNodes],
    write_rels(Rels),
    write_configs(Configs),
    [{releases,Rels},{configs,Configs}].

ins_dists(Nodes,Dists) ->
    [ins_dist(Node,Dists) || Node<-Nodes].

ins_dist({Node,Apps}=Orig,Dists) ->
io:format("~p,~p~n",[Orig,Dists]),
    case lists:keysearch(Node,1,Dists) of
	{value,{Node,Vars}} ->
	    {value,{kernel,Env}}=lists:keysearch(kernel,1,Apps),
	    {Node,lists:keyreplace(kernel,1,Apps,{kernel,Env++Vars})};
	false ->
	    Orig
    end.

mk_rel(_Name,Vsn,Erts,{Node,NApps},Apps) ->
    {release, 	{Node,Vsn},
     {erts, Erts},
     [get_app(App,Apps) || App <- NApps]
    }.

get_app({App,_Env},Apps) ->
    {value,{App,Vsn,_Defs}}=lists:keysearch(App,1,Apps),
    {App,Vsn}.

mk_config({Node,NApps},Apps) ->
    {Node,[mk_env(NApp,Apps) || NApp <- NApps]}.

mk_env({App,Env},Apps) ->
    {value,{App,_VSN,Defs}} = lists:keysearch(App,1,Apps),
    {App,merge_envs(Env,Defs)}.

merge_envs(Env, Defs) ->
    lists:foldl(fun ({Par,_Val}=Def,Acc) ->
			case lists:keysearch(Par,1,Env) of
			    {value,{Par,_V}} ->
				Acc;
			    false ->
				[Def|Acc]
			end
		end, Env, Defs).

mk_dists(Nodes,[]) ->
    [];
mk_dists(Nodes,Dists) ->
    L=lists:flatten([mk_dist(Dist) || Dist <- Dists]),
    [get_dist_info(Node,L) || {Node,_Apps}<-Nodes].

mk_dist({_App,[PNode,AltNodes]}=Def) ->
    [{Node,Def} || Node<-[PNode|tuple_to_list(AltNodes)]].

get_dist_info(Node,L) ->
    Ds=lists:filter(fun({N,_D})-> Node==N end,L),
    Ds1=[D || {_Node,D}<-Ds],
    io:format("Ds1: ~p~n",[Ds1]),
    Ms=mk_mandatory(Node,Ds1),
    {Node,[{distributed,Ds1},{sync_nodes_mandatory,Ms}]}.

mk_mandatory(Node,Dists) ->
    io:format("mk_man: ~p~n",[Dists]),
    L=lists:flatten(
	[[PNode|tuple_to_list(AltNodes)] || 
	    {_App,[PNode,AltNodes]}<-Dists]),
    uniq(Node,L).

uniq(Node,Nodes) ->
    io:format("uniq: ~p~n",[Nodes]),
    S=sets:from_list(Nodes),
    S1=sets:del_element(Node,S),
    sets:to_list(S1).

write_rels(Rels) ->
    lists:foreach(fun write_rel/1,Rels).

write_rel({release,{Node,Vsn},_Erts,_Apps}=Rel) ->
    File=Node++"-"++Vsn++".rel",
    {ok,FD}=file:open(File,[write]),
    io:format(FD,"~p~n",[Rel]),
    file:close(FD).

write_configs(Rels) ->
    lists:foreach(fun write_config/1,Rels).

write_config({Node,Config}) ->
    File=Node++".config",
    {ok,FD}=file:open(File,[write]),
    io:format(FD,"~p~n",[Config]),
    file:close(FD).
