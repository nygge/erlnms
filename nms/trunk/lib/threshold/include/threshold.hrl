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
%% @type th_dest() = #th_dest{id=atom(),string=string(),proc=atom()}.
-record(th_dest,{id,string,proc}).

%% @type th_cmd_key() = {SystemType::atom(),FDN::fdn(),MOClass::atom(),Resolution::duration()}.
%% <ul>
%% <li>STY = atom(), System Type</li>
%% <li>FDN = [RDN]</li>
%% <li>RDN = {atom(),term()}</li>
%% <li>MOC = atom(), Managed Object Class</li>
%% <li>Res = {UNIT,integer()}</li>
%% <li>UNIT = sec|min|hour</li>
%% </ul>

%% @type th_cmd() = #th_cmd{key=th_cmd_key(),cmds=[{atom(),X}]}.
%% <ul>
%% <li>cmds = [Cmd]</li>
%% <li>cmd = id in table th_cmd_def</li>
%% </ul>
-record(th_cmd,{key,cmds}).

%% @type th_prev_key() = {FDN::fdn(),MOClass::atom(),Counter::atom(),Resolution::duration()}.
%% <ul>
%% <li>FDN = [RDN]</li>
%% <li>RDN = {atom,term}</li>
%% <li>MOC = atom, Managed Object Class</li>
%% <li>Cnt = atom, Counter name</li>
%% <li>Res = {UNIT,int}</li>
%% </ul>

%% @type th_prev() = #th_prev{key=th_prev_key(),val=atom()}.
%% val = atom, previous threshold level
-record(th_prev,{key,val}).

%% @type th_threshold_key() = {FDN::fdn(),Resolution::duration(),Counter::atom()}.
%% <ul>
%% <li>FDN = managed element name</li>
%% <li>Resolution = resolution</li>
%% <li>Counter = counter id</li>
%% </ul>

%% @type th_threshold() = #th_threshold{key=th_threshold_key(),min=integer()|float(),max=integer()|float(),dests=[dest()]}.
%% <ul>
%% <li>min = lower limit</li>
%% <li>max = upper limit</li>
%% <li>dests = [dest]</li>
%% <li>dest = {dest_id,pars}</li>
%% <li>dest_id = id in th_dest</li>
%% <li>pars = term, extra info included in threshold crossing message</li>
%% </ul>
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
