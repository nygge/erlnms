%%%-------------------------------------------------------------------
%%% File    : fast_agi.erl
%%% Author  : anders <anders@>
%%% Description : 
%%%
%%% Created : 24 Feb 2006 by anders <anders@>
%%%-------------------------------------------------------------------
-module(fast_agi).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% API

-export([answer/1,
	 channel_status/1,
	 channel_status/2,
	 db_del/3,
	 db_del_tree/2,
	 db_del_tree/3,
	 db_get/3,
	 db_put/4,
	 exec/3,
	 get_data/4,
	 get_full_variable/2,
	 get_full_variable/3,
	 get_option/3,
	 get_option/4,
	 get_variable/2,
	 hangup/1,
	 noop/1,
	 noop/2,
	 receive_char/2,
	 record_file/5,
	 say_alpha/3,
	 say_date/3,
	 say_datetime/3,
	 say_digits/3,
	 say_number/2,
	 say_number/3,
	 say_phonetic/3,
	 say_time/3,
	 send_image/2,
	 send_text/2,
	 set_autohangup/2,
	 set_callerid/2,
	 set_context/2,
	 set_extension/2,
	 set_music_on/2,
	 set_music_on/3,
	 set_priority/2,
	 set_variable/3,
	 stream_file/3,
	 tdd_mode/2,
	 verbose/3,
	 wait_for_digit/2]).

-record(c,  {sock,
             port,
             peer_addr,
             peer_port
	     }).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case sup_fast_agi:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%%====================================================================
%% API
%%====================================================================

%% @spec answer(C) -> Result
%%    C      = Session
%%    Result = Success | Failure
%%    Success= 0
%%    Failure= -1
%%
%% @doc Answers the channel (if it is not already in an answered state).
%% @end
answer(C) ->
    send(C,"ANSWER\n").

%% @spec channel_status(C) -> Result
%%    C      = Session
%%    Result = integer
%%
%% @doc Get the status of the current channel.
%% @end
channel_status(C)->
    send(C,"CHANNEL STATUS\n").

%% @spec channel_status(C,Channel) -> Result
%%    C      = Session
%%    Channel= String
%%    Result = integer
%%
%% @doc Get the status of the specified channel.
%% @end
channel_status(C,Channel)->
    send(C,["CHANNEL STATUS ",Channel,"\n"]).

%% @spec db_del(C,Family,Key) -> Result
%%    C      = Session
%%    Family = string
%%    Key    = string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Delete the entry from the Asterisk DB given by Family and Key.
%% @end
db_del(C,Family,Key) ->
    send(C,["DATABASE DEL ",Family," ",Key,"\n"]).

%% @spec db_del_tree(C,Family) -> Result
%%    C      = Session
%%    Family = string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Deletes the Family from the Asterisk DB.
%% @end
db_del_tree(C,Family) ->
    send(C,["DATABASE DELTREE ",Family,"\n"]).

%% @spec db_del_tree(C,Family,Keytree) -> Result
%%    C      = Session
%%    Family = string
%%    Keytree= string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Deletes the Family from the Asterisk DB.
%% @end
db_del_tree(C,Family,Keytree) ->
    send(C,["DATABASE DELTREE ",Family," ",Keytree,"\n"]).

%% @spec db_get(C,Family,Key)-> Result
%%    C      = Session
%%    Family = string
%%    Key    = string
%%    Result = notset | {value,Value}
%%
%% @doc Get the value from the Asterisk DB.
%% @end
db_get(C,Family,Key)->
    send(C,["DATABASE GET ",Family," ",Key,"\n"]).

%% @spec db_put(C,Family,Key,Value) -> Result
%%    C      = Session
%%    Family = string
%%    Key    = string
%%    Value  = string
%%    Result = Success | Failure
%%    Success= 1
%%    Failure= 0
%%
%% @doc Adds or updates an entry in the Asterisk database for the 
%%      specified family and key, with the specified value.
%% @end
db_put(C,Family,Key,Value) ->
    send(C,["DATABASE PUT ",Family," ",Key," ",Value,"\n"]).

%% @spec exec(C,Appl,Opts) -> Result
%%    C      = Session
%%    Appl   = string
%%    Opts   = string
%%    Result = not_found|{result,Retval}
%%    Retval = string
%%
%% @doc Execute a dialplan application.
%% @end
exec(C,Appl,Opts) ->
    send(C,["EXEC ",Appl," ",Opts,"\n"]).

%% @spec get_data(C,File,Timeout,MaxDigits) -> Result
%%    C      = Session
%%    File   = string
%%    Timeout= integer
%%    MaxDigits=integer
%%    Result = string
%%
%% @doc Play the audio file File and accept up to MaxDigits DTMF digits.
%% @end
get_data(C,File,Timeout,MaxDigits) ->
    send(C,["GET DATA ",File," ",integer_to_list(Timeout)," ",
	    integer_to_list(MaxDigits),"\n"]).

%% @spec get_full_variable(C,Var) -> Result
%%    C      = Session
%%    Var    = string
%%    Result = error|{value,Value}
%%    Value  = string
%%
%% @doc Get the value of Var.
%% @end
get_full_variable(C,Var) ->
    send(C,["GET FULL VARIABLE ",Var,"\n"]).

%% @spec get_full_variable(C,Var,Channel) -> Result
%%    C      = Session
%%    Var    = string
%%    Chnnel = string
%%    Result = error|{value,Value}
%%    Value  = string
%%
%% @doc Get the value of Var.
%% @end
get_full_variable(C,Var,Channel) ->
    send(C,["GET FULL VARIABLE ",Var," ",Channel,"\n"]).

%% @spec get_option(C,File,Escape) -> Result
%%    C      = Session
%%    Result = digits
%%
%% @doc Same as stream_file.
%% @end
get_option(C,File,Escape) ->
    send(C,["GET OPTION ",File," ",Escape,"\n"]).

%% @spec get_option(C,File,Escape,Timeout) -> Result
%%    C      = Session
%%    Result = digits
%%
%% @doc Same as stream_file but with a timeout in seconds.
%% @end
get_option(C,File,Escape,Timeout) ->
    send(C,["GET OPTION ",File," ",Escape," ",integer_to_list(Timeout),"\n"]).

%% @spec get_variable(C,Var) -> Result
%%    C      = Session
%%    Var    = string
%%    Result = error|{value,Value}
%%
%% @doc Get the value of Var.
%% @end
get_variable(C,Var) ->
    send(C,["GET VARIABLE ",Var,"\n"]).

%% @spec hangup(C) -> Result
%%    C      = Session
%%    Result = success|failure
%%
%% @doc Hangup the current channel.
%% @end
hangup(C) ->
    send(C,"HANGUP\n").

%% @spec hangup(C,Channel) -> Result
%%    C      = Session
%%    Channel= string
%%    Result = success|failure
%%
%% @doc Hangup the specified channel.
%% @end
hangup(C,Channel) ->
    send(C,["HANGUP ",Channel,"\n"]).

%% @spec noop(C) -> Result
%%    C      = Session
%%    Result = ok
%%
%% @doc Does nothing
%% @end
noop(C) ->
    send(C,"NOOP\n").

%% @spec noop(C,Text) -> Result
%%    C      = Session
%%    Text   = string
%%    Result = ok
%%
%% @doc Does nothing, but prints Text on the Asterisk console.
%% @end
noop(C,Text) ->
    send(C,["NOOP ",Text,"\n"]).

%% @spec receive_char(C,Timeout) -> Result
%%    C      = Session
%%    Timeout=integer
%%    Result = what
%%
%% @doc Receive a character on the current channel.
%% @end
receive_char(C,Timeout) ->
    send(C,["RECEIVE CHAR ",integer_to_list(Timeout),"\n"]).

%% @spec record_file(C,File,Format,Escape,Timeout) -> Result
%%    C      = Session
%%    File   = string
%%    Format = string
%%    Escape = string
%%    Timeout= integer
%%    Result = what
%%
%% @doc Record audio.
%% @end
record_file(C,File,Format,Escape,Timeout) ->
    send(C,["RECORD FILE ",File," ",Format," ",Escape," ",
	    integer_to_list(Timeout),"\n"]).

%% @spec say_alpha(C,Number,Escape) -> Result
%%    C      = Session
%%    Number = integer
%%    Escape = string
%%    Result = what
%%
%% @doc 
%% @end
say_alpha(C,Number,Escape) ->
    send(C,["SAY ALPHA ",integer_to_list(Number)," ",Escape,"\n"]).

%% @spec say_date(C,Date,Escape) -> Result
%%    C      = Session
%%    Date   = integer
%%    Escape = string
%%    Result = what
%%
%% @doc Says the given date. The Date is given as a UNIX time, i.e. number of secunds since 00:00:00 on January 1, 1970.
%% @end
say_date(C,Date,Escape) ->
    send(C,["SAY DATE ",integer_to_list(Date)," ",Escape,"\n"]).

%% @spec say_datetime(C,Datetime,Escape) -> Result
%%    C      = Session
%%    Datetime= integer
%%    Escape = string
%%    Result = what
%%
%% @doc Says the given datetime. The Datetime is given as a UNIX time, i.e. 
%%      number of secunds since 00:00:00 on January 1, 1970.
%% @end
say_datetime(C,Datetime,Escape) ->
    send(C,["SAY DATETIME ",integer_to_list(Datetime)," ",Escape,"\n"]).

%% @spec say_digits(C,Number,Escape) -> Result
%%    C      = Session
%%    Number = integer
%%    Escape = string
%%    Result = what
%%
%% @doc Say digits.
%% @end
say_digits(C,Number,Escape) ->
    send(C,["SAY DIGITS ",integer_to_list(Number)," ",Escape,"\n"]).

%% @spec say_number(C,Number) -> Result
%%    C      = Session
%%    Number = integer
%%    Result = what
%%
%% @doc Say number.
%% @end
say_number(C,Number) ->
    say_number(C,integer_to_list(Number),"\"\"").

%% @spec say_number(C,Number,Escape) -> Result
%%    C      = Session
%%    Number = integer
%%    Escape = string
%%    Result = what
%%
%% @doc Say number.
%% @end
say_number(C,Num,Escape) when is_list(Escape)->
    send(C,["SAY NUMBER ",integer_to_list(Num)," ", Escape, "\n"]).

%% @spec say_phonetic(C,String,Escape) -> Result
%%    C      = Session
%%    String = string
%%    Result = what
%%
%% @doc Say string with phonetics.
%% @end
say_phonetic(C,String,Escape) ->
    send(C,["SAY PHONETIC ",String," ",Escape,"\n"]).

%% @spec say_time(C,Time,Escape) -> Result
%%    C      = Session
%%    Time   = integer
%%    Escape = string
%%    Result = what
%%
%% @doc Say time. The Time is given as a UNIX time, i.e. 
%%      number of secunds since 00:00:00 on January 1, 1970.
%% @end
say_time(C,Time,Escape) ->
    send(C,["SAY TIME ",integer_to_list(Time)," ",Escape,"\n"]).

%% @spec send_image(C,Image) -> Result
%%    C      = Session
%%    Image  = string
%%    Result = what
%%
%% @doc Send an image on the current channel.
%% @end
send_image(C,Image) ->
    send(C,["SEND IMAGE ",Image,"\n"]).

%% @spec send_text(C,Text) -> Result
%%    C      = Session
%%    Text   = string
%%    Result = what
%%
%% @doc Send a text on the current channel.
%% @end
send_text(C,Text) ->
    send(C,["SEND TEXT ",Text,"\n"]).

%% @spec set_autohangup(C,Time) -> Result
%%    C      = Session
%%    Time   = integer
%%    Result = what
%%
%% @doc Set the channel to hangup after Time seconds.
%% @end
set_autohangup(C,Time) ->
    send(C,["SET AUTOHANGUP ",integer_to_list(Time),"\n"]).

%% @spec set_callerid(C,Number) -> Result
%%    C      = Session
%%    Number = string
%%    Result = what
%%
%% @doc Set callerid for the current channel.
%% @end
set_callerid(C,Number) ->
    send(C,["SET CALLERID ",Number,"\n"]).

%% @spec set_context(C,Context) -> Result
%%    C      = Session
%%    Context= string
%%    Result = what
%%
%% @doc Set the context for continuation upon exiting the AGI application.
%% @end
set_context(C,Context) ->
    send(C,["SET CONTEXT ",Context,"\n"]).

%% @spec set_extension(C,Extension) -> Result
%%    C      = Session
%%    Extension=string
%%    Result = what
%%
%% @doc Changes the extension for continuation upon exiting the AGI application.
%% @end
set_extension(C,Extension) ->
    send(C,["SET EXTENSION ",Extension,"\n"]).

%% @spec set_music_on(C,State) -> Result
%%    C      = Session
%%    State  = on|off
%%    Result = what
%%
%% @doc Enable/disable the Music on Hold generator.
%% @end
set_music_on(C,State) ->
    send(C,["SET MUSIC ON ",State,"\n"]).

%% @spec set_music_on(C,State,Class) -> Result
%%    C      = Session
%%    State  = on|off
%%    Class  = string
%%    Result = what
%%
%% @doc Enable/disable the Music on Hold generator.
%% @end
set_music_on(C,State,Class) ->
    send(C,["SET MUSIC ON ",State," ",Class,"\n"]).

%% @spec set_priority(C,Priority) -> Result
%%    C      = Session
%%    Priority=string
%%    Result = what
%%
%% @doc Changes the priority for continuation upon exiting the AGI application.
%% @end
set_priority(C,Priority) ->
    send(C,["SET PRIORITY ",Priority,"\n"]).

%% @spec set_variable(C,Variable,Value) -> Result
%%    C      = Session
%%    Variable = string
%%    Value  = string
%%    Result = what
%%
%% @doc Sets or updates the value of Variable.
%% @end
set_variable(C,Variable,Value) ->
    send(C,["SET VARIABLE ",Variable," ",Value,"\n"]).

%% @spec stream_file(C,File,Escape) -> Result
%%    C      = Session
%%    File   = string
%%    Escape = string
%%    Result = what
%%
%% @doc Play audio file indicated by File.
%% @end
stream_file(C,File,Escape) ->
    send(C,["STREAM FILE ",File," ", Escape, "\n"]).

%% @spec tdd_mode(C,Mode) -> Result
%%    C      = Session
%%    Mode   = on|off
%%    Result = what
%%
%% @doc Enable/disable TDD on this channel.
%% @end
tdd_mode(C,Mode) ->
    send(C,["TDD MODE ",atom_to_list(Mode),"\n"]).

%% @spec verbose(C,Message,Level) -> Result
%%    C      = Session
%%    Message= string
%%    Level  = string
%%    Result = what
%%
%% @doc Sends Message to console via the verbose message system.
%% @end
verbose(C,Message,Level) ->
    send(C,["VERBOSE",Message," ",Level,"\n"]).

%% @spec wait_for_digit(C,Timeout) -> Result
%%    C      = Session
%%    Timeout= integer
%%    Result = what
%%
%% @doc Wait for a DTMF digit on the current channel. The timeout is in milliseconds.
%% @end
wait_for_digit(C,Timeout) ->
    send(C,["WAIT FOR DIGIT",integer_to_list(Timeout),"\n"]).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

send(C,Pack) ->
    io:format("sending ~p~n",[Pack]),
    gen_tcp:send(C#c.sock,Pack),
    check_result(C).

check_result(C) ->
    {ok,R}=gen_tcp:recv(C#c.sock, 0, 30000),
    io:format("result ~p~n",[R]),
    Res=string:sub_word(R,1,$\n),
    [list_to_integer(string:sub_word(Res,1)),
     list_to_integer(string:sub_word(string:sub_word(Res,2),2,$=)),
     string:sub_word(Res,3)].
