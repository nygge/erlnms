-module(test).

-export([obj_cre/0,obj_cre/1,obj_cre/2]).

obj_cre() ->
    alarm_sts_server!{obj_cre_evt,me,[{ne,'host.domain.com'}],up,[]}.

obj_cre(ME) ->
    alarm_sts_server!{obj_cre_evt,me,ME,up,[]}.

obj_cre(Class,Name) ->
    alarm_sts_server!{obj_cre_evt,Class,Name,up,[]}.
