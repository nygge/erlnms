-record('CIM_SNMPTrapIndication',{'IndicationIdentifier',
				  'CorrelatedIndications',
				  'IndicationTime',
                                  enterprise,	% sysObjectId
                                  agentAddress,
				  genericTrap,
				  specificTrap,
				  timeStamp,	% time since last restart
				  varBindNames,
				  varBindSyntaxes,
				  varBindValues}).

% GenericTrap Values
-define(COLDSTART,0).
-define(WARMSTART,1).
-define(LINKDOWN,2).
-define(LINKUP,3).
-define(AUTHENTICATIONFAILURE,4).
-define(EQPNEIGHBORLOSS,5).
-define(ENTERPRISESPECIFIC,6).

% VarBindSyntaxes Values
-define(INTEGER,1).
-define(OCTETSTRING,2).
-define(OBJECTIDENTIFIER,3).
-define(NETWORKADDRESS,4).
-define(COUNTER,5).
-define(GAUGE,6).
-define(TIMETICKS,7).
-define(OPAQUE,8).

% ifAdminStatus and ifOperStatus
-define(UP,1).
-define(DOWN,2).
-define(TESTING,3).

% ifOperStatus
-define(OpSt_UNKNOWN,4).
-define(DORMANT,5).
