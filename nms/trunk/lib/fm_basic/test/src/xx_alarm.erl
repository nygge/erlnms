-module(xx_alarm).

-export([send/1]).

-include("CIM_AlertIndication.hrl").

send(ME) ->
    X=#'CIM_AlertIndication'{'AlertingManagedElement'=ME},
    raw_alarm:send(X).
