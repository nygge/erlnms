-module(unblock_link).
-behaviour(gen_server).

-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).

-record(state,{val=[]}).

%%=======================================================================
%%	Interface
%%
start(Pars)-> 
   gen_server:start({local, ?MODULE}, ?MODULE, [Pars], []).
start_link(Pars)-> 
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Pars], []).
%stop()-> 
   %gen_server:call(?MODULE, stop, 10000).

%%=======================================================================
%%      Callbacks
%%
init(Pars)->
   process_flag(trap_exit, true),
   self()!{start,Pars},
   {ok, #state{}}.

handle_call(stop,From,State)->
   {stop,normal,ok,State};

handle_call(Msg,From,State)->
   {reply,ok,State}.

handle_cast(Msg,State)->
   {noreply,State}.

handle_info({start,Pars},State)->
   io:format("unblock_link: ~w~n",[Pars]),
   {stop,normal,State};

handle_info(Info,State)->
   {noreply,State}.

terminate(Reason,State)->
   ok.

code_change(OldVsn,State,Extra)->
   {ok,State}.

%%=======================================================================
%%      Implementation
%%


