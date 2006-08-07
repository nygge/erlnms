%%%-------------------------------------------------------------------
%%% File    : rrdtool.hrl
%%% % @author Anders Nygren <anders.nygren@gmail.com>
%%% % @copyright 2004-2006 Anders Nygren
%%% % @version {@vsn}
%%% % @doc RRDTool interface library.
%%% % @end
%%%
%%% Created :  5 Feb 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------

%% @type duration() = {Unit::timeunit(),Value::integer()}

%% @type timeunit() = sec | min | hour | day | week | month | year

%% @type cf() = 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'. 
%% Consolidation function, defines how datapoints are consolidated into a new datapoint with a longer time interval.
%%%
%% @type ds_type() = 'GAUGE' | 'COUNTER' | 'DERIVE' | 'ABSOLUTE'.
%% Type of data source.


%% @type rrd_file() = #rrd_file{file=Name::string(),start=datetime(),step=duration(),dss=[rrd_dss()],rras=[rrd_rra_def()],last=datetime()}.
%% RRD file definition
%% <ul>
%% <li>file = Full path of file</li>
%% <li>start = Start time of file</li>
%% <li>step = Time between updates</li>
%% <li>dss = Datasources</li>
%% <li>rras = Round robin archives</li>
%% <li>last = time of latest update, NOT used when creating an RRD file.</li>
%% </ul>
-record(rrd_file,{file,   % string, name of the file
		  start,  % Start of file
		  step,   % Duration
		  dss,    % [rrd_dss]
		  rras,   % [rrd_rra]
		  last    % Timestamp of last update
		 }).

%% @type rrd_ds() = #rrd_ds{name=atom(),type='GAUGE' | 'COUNTER' | 'DERIVE' | 'ABSOLUTE',hb=Heartbeat::integer(),min=integer(),max=integer(),last=datetime(),value=float(),unknown_sec=integer()}.
%% Data source definition.
%% <ul>
%% <li>name = Data source name</li>
%% <li>type = Type of value</li>
%% <li>hb = heartbeat, the maximum number of seconds between two updates</li>
%% <li>min = minimum allowed value</li>
%% <li>max = maximum allowed value</li>
%% <li>last = timestamp for latest value</li>
%% <li>value = latest value</li>
%% <li>unknown_sec = number of seconds for which no value is known</li>
%% </ul>

-record(rrd_ds,{name,       % Data Source Name, 
		type,       % 'GAUGE' | 'COUNTER' | 'DERIVE' | 'ABSOLUTE'
		hb,         % Max number of seconds between two updates
		min,        % integer, Min allowed value
		max,        % integer, Max allowed value
		last,       % Not for create command
		value,      % Not for create command
		unknown_sec % Not for create command
	       }).

%% @type rrd_ds_comp() = #rrd_ds_comp{name=atom(),rpn=string()}.
%% Computed datasource
%% <ul>
%% <li>name = Data source name</li>
%% <li>rpn = RPN expression for calculating the computed datasource value</li>
%% </ul>
-record(rrd_ds_comp,{name,  % Data Source Name, 
		     rpn    % RPN expression for calculating DS value
		    }).

%%------------------------------------------------
%% Round Robin Archive definition
%%
%% RRA = {'CF',XFF,ConsInt,ArchiveTime}
%% CF          = Consolidation Function
%%             = 
%% XFF         = 
%% ConsInt     = 
%% ArchiveTime = 
%% Unit        = sec | min | hour | day | week | month | year
%% No          = integer

%% @type rrd_rra_def() = rrd_rra() | rrd_rra_hwpred() | rrd_rra_seasonal() |rrd_rra_devseasonal() |rrd_rra_devpredict() |rrd_rra_failures() 

%% @type rrd_rra() = #rrd_rra{id=atom(),cf=cf(),xff=float(),interval=duration(),duration=duration()}.
%% <ul>
%% <li>id = </li>
%% <li>cf = Consolidation function</li>
%% <li>xff = X-Files Factor, 0&lt;XFF&lt;1</li>
%% <li>interval = Consolidation interval</li>
%% <li>duration = Duration the archive contains</li>
%% </ul>

-record(rrd_rra, {id,       % 
		  cf,       % 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'
		  xff,      % X-Files Factor, 0<XFF<1
		  interval, % {Unit,No}, Consolidation Interval
		  duration  % {Unit,No}, Duration the archive contains
		 }).

%% @type rrd_rra_hwpred() = #rrd_rra_hwpred{id=integer(),rows=integer(),alpha=float(),beta=float(),period=integer(),rra_num=integer()}.
%% <ul>
%% <li> = </li>
%% </ul>
-record(rrd_rra_hwpred,{id,
			rows,
			alpha,
			beta,
			period,
			rra_num
		       }).

%% @type rrd_rra_seasonal() = #rrd_rra_seasonal{id=integer(),period=integer(),gamma=float(),rra_num=integer()}.
%% <ul>
%% <li> = </li>
%% </ul>
-record(rrd_rra_seasonal,{id,
			  period,
			  gamma,
			  rra_num
			 }).

%% @type rrd_rra_devseason() = #rrd_rra_devseason{id=integer(),period=integer(),gamma=float(),rra_num=integer()}.
%% <ul>
%% <li> = </li>
%% </ul>
-record(rrd_rra_devseason,{id,
			   period,
			   gamma,
			   rra_num
			  }).

%% @type rrd_rra_devpredict() = #rrd_rra_devpredict{id=integer(),rows=integer(),rra_num=integer()}.
%% <ul>
%% <li> = </li>
%% </ul>
-record(rrd_rra_devpredict,{id,
			    rows,
			    rra_num
			   }).

%% @type rrd_rra_failures() = #rrd_rra_failures{id=integer(),rows=integer(),threshold=integer(),window=integer(),rra_num=integer()}.
%% <ul>
%% <li> = </li>
%% </ul>
-record(rrd_rra_failures,{id,
			  rows,
			  threshold,
			  window,
			  rra_num
			 }).


%%------------------------------------------------
%% records for graph and export
%%

%% rrd_export
%% @type rrd_export() = #rrd_export{start=datetime(),stop=datetime(),rows=integer(),step=integer(),defs=[Def::rra_def()],cdefs=[CDef::rrd_cdef()],xports=[rrd_xport()]}.
%% Export command.
%% <ul>
%% <li>Start = Start time of export</li>
%% <li>Stop = End time of export</li>
%% <li>Rows = Number of rows to return</li>
%% <li>Step = Time between datapoint</li>
%% <li>Defs = </li>
%% <li>CDefs = </li>
%% <li>xports = </li>
%% </ul>

-record(rrd_export,{start,   % Start time
		    stop,    % End time
		    rows,    % Max number of rows
		    step,    % Data point step
		    defs,    % [rrd_def]
		    cdefs,   % [rrd_cdef]
		    xports   % [rrd_xport]
		   }).

%% @type rrd_xport() = #rrd_xport{vname=atom(),legend=string()}.
%% Variable that is actually exported by export command.
%% <ul>
%% <li>VarName = Name of variable to export </li>
%% <li>Legend = Text label associated with the exported variable</li>
%% </ul>

-record(rrd_xport,{vname,    % Variable name
		   legend    % Exported name
		  }).

%% Common for rrd_graph and rrd_xport

%% @type rrd_def() = #rrd_def{vname=atom(),rrd=string(),ds_name=atom(),cf='AVERAGE' | 'MIN' | 'MAX' | 'LAST'}.
%% <ul>
%% <li>vname = Name of variable to export </li>
%% <li>rrd = File name</li>
%% <li>ds_name = Datasource name</li>
%% <li>cf = Consolidation function</li>
%% </ul>

-record(rrd_def,{vname,      % Variable name
		 rrd,        % RRD filename
		 ds_name,    % Datasource name
		 cf          % Consolidation functions
		}).

%% @type rrd_cdef() = #rrd_cdef{vname=atom(),rpn=RPNexpression::string()}.
%% <ul>
%% <li>vname = Variable name</li>
%% <li>rpn = RPN expression</li>
%% </ul>

-record(rrd_cdef,{vname,     % Variable name
		  rpn        % RPN expression
		 }).

%% @type rrd_vdef() = #rrd_vdef{vname=atom(),rpn=RPNexpression::string()}.
%% <ul>
%% <li>vname = Variable name</li>
%% <li>rpn = RPN expression</li>
%% </ul>
-record(rrd_vdef,{vname,     % Variable name
		  rpn        % RPN expression
		 }).

%% rrd_graph
%% @type rrd_graph() = #rrd_graph{file=string(),start=datetime(),stop=datetime(),step=duration(),defs=[rrd_def()],cdefs=[rrd_cdef()],vdefs=[rrd_vdef()],graph=[gelement()],options=[options()]}.
%% <ul>
%% <li>file = Full path to file</li>
%% <li>start = starttime for graph</li>
%% <li>stop = endtime of graph</li>
%% <li>step = time between each point in the graph</li>
%% <li>defs = definitions of variabels</li>
%% <li>cdefs = definitions of calculated variables</li>
%% <li>vdefs = definitions of ???</li>
%% <li>graph = graph elements</li>
%% <li>options = options, duh!</li>
%% </ul>
-record(rrd_graph,{file,start,stop,step,defs=[],cdefs=[],vdefs=[],graph=[],options=[]}).

%% @type gelement() = rrd_print() | rrd_comment() | rrd_vrule() | rrd_line() | rrd_area() | rrd_tick() | rrd_shift()

%% @type rrd_print() = #rrd_print{vname=atom(),format=string()}.
%% <ul>
%% <li>vname = Variable name</li>
%% <li>format = format string</li>
%% </ul>
-record(rrd_print,{vname,format}).

%% @type rrd_gprint() = #rrd_gprint{vname=atom(),format=string}.
%% <ul>
%% <li>vname = Variable name</li>
%% <li>format = format string</li>
%% </ul>
-record(rrd_gprint,{vname,format}).

%% @type rrd_comment() = #rrd_comment{text=string()}.
%% <ul>
%% <li>text = Comment</li>
%% </ul>
-record(rrd_comment,{text}).

%% @type rrd_vrule() = #rrd_vrule{time=datetime() | atom(),color=rgb(),legend=string()}.
%% <ul>
%% <li>time = </li>
%% <li>color = rgb color value</li>
%% <li>legend = text</li>
%% </ul>
-record(rrd_vrule,{time,color,legend}).

%% @type rrd_line() = #rrd_line{width=integer() | float(),vname=atom() , color=rgb(),legend=string(),stack=boolean()}.
%% <ul>
%% <li>widht = widht of line</li>
%% <li>vname = Variable name</li>
%% <li>color = rgb color value</li>
%% <li>legend = Text</li>
%% <li>stack = </li>
%% </ul>
-record(rrd_line,{width,vname,color,legend,stack}).

%% @type rrd_area() = #rrd_area{vname=VarName::atom(),color=rgb(),legend=string(),stack=boolean()}.
%% <ul>
%% <li>vname = Variable name</li>
%% <li>color = rgb color value</li>
%% <li>legend = Text</li>
%% <li>stack = </li>
%% </ul>
-record(rrd_area,{vname,color,legend,stack}).

%% @type rrd_tick() = #rrd_tick{vname=VarName::atom(),color=rgb(),aa=integer(),fraction=float(),legend=string()}.
%% <ul>
%% <li>vname = Variable name</li>
%% <li>color = rgb color value</li>
%% <li>aa = </li>
%% <li>legend = Text</li>
%% </ul>
-record(rrd_tick,{vname,color,aa,fraction,legend}).

%% @type rrd_shift() = #rrd_shift{vname=VarName::atom(),offset=atom()|integer()}.
%% <ul>
%% <li>vname = Variable name</li>
%% <li>offset = nummber of seconds the graph is offset</li>
%% </ul>
-record(rrd_shift,{vname,offset}).

%% @type options() = [option()].
%% @type option()  = {start,T} | {'end',T} | {x_grid,P} | {y_grid,P} | 
%%             alt_y_grid | alt_y_mrtg | alt_autoscale | 
%%             alt_autoscale_max | {units_exponent,V} | 
%%             {vertical_label,V} | {width,W} | {height,H} | 
%%             interlaced | {imginfo,F} | {imgformat,F} | {background,V} | 
%%             {overlay,V} | {unit,V} | lazy | {upper_limit,V} |
%%             {lowerlimit,V} | rigid | {base,V} | logarithmic | {color,V} | 
%%             no_legend | {title,T} | {step,V}.
