%% Record definition for Trap to Alarm mapping used by
%% trap2alarm.
%% Key = {NE,SysObjId,Trap}
%% NE  = atom | '*'
%% Trap= {generic,Val} | {specific,Val} 
%% Varbinds = [Conds]
%% Cond = {Pos,Val}, Pos,Val = integer
%% PC = integer
%% PCD = {al_text,{pcd,Num}}
%% PRA = {al_text,{pra,Num}}
%% FUN = {Mod,Fun/2}, Fun(Trap) -> FDN

-record(trap2alarm,{key,varbinds,at,sev,pc,pcd,pra,'fun'}).
