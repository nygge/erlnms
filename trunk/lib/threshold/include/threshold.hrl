%%%-------------------------------------------------------------------
%%% File    : threshold.hrl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  6 Feb 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------

%% dest
%% id = atom
%% string = Printable name of destination
%% proc = atom, registered name of receiving process
-record(th_dest,{id,string,proc}).

%% key = {STY,FDN,MOC,Res}
%% STY = atom, System Type
%% FDN = [RDN]
%% RDN = {atom,term}
%% MOC = atom, Managed Object Class
%% Res = {UNIT,int}
%% UNIT = sec|min|hour
%% cmds = [Cmd]
%% cmd = id in table th_cmd_def
-record(th_cmd,{key,cmds}).

%% key = {FDN,MOC,Cnt,Res}
%% FDN = [RDN]
%% RDN = {atom,term}
%% MOC = atom, Managed Object Class
%% Cnt = atom, Counter name
%% Res = {UNIT,int}
%% val = atom, previous threshold level
-record(th_prev,{key,val}).

%% key = {me,res,cnt}
%% me = managed element name,
%% res = resolution,
%% cnt = counter id
%% min = lower limit
%% max = upper limit
%% dests = [dest]
%% dest = {dest_id,pars}
%% dest_id = id in th_dest,
%% pars = term, extra info included in threshold crossing message
-record(th_threshold,{key,min,max,dests}).

%% me = managed element,
%% cnt = counter name,
%% time = time stamp for counter value,
%% res = resolution,
%% val = current counter value,
%% min = lower limit for threshold,
%% max = upper limit for threshold,
%% extra = extra parameters from th_threshold.pars
-record(th_message,{me,cnt,time,res,val,min,max,extra}).
