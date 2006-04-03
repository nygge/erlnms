-record('CIM_AlertIndication',{'IndicationIdentifier',
			       'CorrelatedIndications'=[],
			       'IndicationTime',
                               'Description',
			       'AlertingManagedElement',
			       'AlertType', 
			       'OtherAlertType'="", 
			       'PerceivedSeverity',
			       'OtherSeverity'="",
			       'ProbableCause',
			       'ProbableCauseDescription'="",
			       'Trending',
			       'RecommendedActions'=[],
			       'EventId'="",
			       'EventTime',
			       'SystemCreationClassName'="",
			       'SystemName'="",
			       'ProviderName'=""}).

-record('CIM_ThresholdIndication',{'CIM_AI',
                                   'ThresholdIdentifier',
                                   'ThresholdValue',
				   'ObservedValue'}).

% AlertType
-define(OTHER,1).
-define(COMMUNICATIONALERT,2).
-define(QOSALERT,3).
-define(PROCESSINGERROR,4).
-define(DEVICEALERT,5).
-define(ENVIRONMENTALALERT,6).
-define(MODELCHANGE,7).
-define(SECURITYALERT,8).

% PerceivedSeverity
-define(UNKNOWN,0).
%-define(OTHER,1).	% Already Defined
-define(INFORMATION,2).
-define(DEGRADED,3).
-define(MINOR,4).
-define(MAJOR,5).
-define(CRITICAL,6).
-define(FATAL,7).
-define(CEASE,8).

% Trending
%-define(UNKNOWN,0).	% Already defined
-define(NOTAPPLICABLE,1).
-define(TRENDINGUP,2).
-define(TRENDINGDOWN,3).
-define(NOCHANGE,4).

%NLS ids for CIM_AlertIndication attributes
-define(INDICATIONIDENTIFIER,1).
-define(CORRELATEDINDICATIONS,2).
-define(INDICATIONTIME,3).
-define(DESCRIPTION,4).
-define(ALERTINGMANAGEDELEMENT,5).
-define(ALERTTYPE,6).
-define(OTHERALERTTYPE,7).
-define(PERCEIVEDSEVERITY,8).
-define(OTHERSEVERITY,9).
-define(PROBABLECAUSE,10).
-define(PROBABLECAUSEDESCRIPTION,11).
-define(TRENDING,12).
-define(RECOMMENDEDACTIONS,13).
-define(EVENTID,14).
-define(EVENTTIME,15).
-define(SYSTEMCREATIONCLASSNAME,16).
-define(SYSTEMNAME,17).
-define(PROVIDERNAME,18).

%NLS ids for CIM_ThresholdIndication attributes
-define(THRESHOLDIDENTIFIER,1).
-define(THRESHOLDVALUE,2).
-define(OBSERVEDVALUE,3).
