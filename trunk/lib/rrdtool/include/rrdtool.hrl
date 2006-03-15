%%%-------------------------------------------------------------------
%%% File    : rrdtool.hrl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Feb 2004 by Anders Nygren <anders.nygren@gmail.com>
%%%-------------------------------------------------------------------

%% Duration    = {Unit,Integer}
%% Unit        = sec | min | hour | day | week | month | year

-record(rrd_file,{file,   % string, name of the file
		  start,  % Start of file
		  step,   % Duration
		  dss,    % [rrd_dss]
		  rras,   % [rrd_rra]
		  last    % Timestamp of last update
		 }).

-record(rrd_ds,{name,       % Data Source Name, 
		type,       % 'GAUGE' | 'COUNTER' | 'DERIVE' | 'ABSOLUTE'
		hb,         % Max number of seconds between two updates
		min,        % integer, Min allowed value
		max,        % integer, Max allowed value
		last,       % Not for create command
		value,      % Not for create command
		unknown_sec % Not for create command
	       }).

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

-record(rrd_rra, {id,       % 
		  cf,       % 'AVERAGE' | 'MIN' | 'MAX' | 'LAST'
		  xff,      % X-Files Factor, 0<XFF<1
		  interval, % {Unit,No}, Consolidation Interval
		  duration  % {Unit,No}, Duration the archive contains
		 }).

-record(rrd_rra_hwpred,{id,
			rows,
			alpha,
			beta,
			period,
			rra_num
		       }).

-record(rrd_rra_seasonal,{id,
			  period,
			  gamma,
			  rra_num
			 }).

-record(rrd_rra_devseason,{id,
			   period,
			   gamma,
			   rra_num
			  }).

-record(rrd_rra_devpredict,{id,
			    rows,
			    rra_num
			   }).

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
-record(rrd_export,{start,   % Start time
		    stop,    % End time
		    rows,    % Max number of rows
		    step,    % Data point step
		    defs,    % [rrd_def]
		    cdefs,   % [rrd_cdef]
		    xports   % [rrd_xport]
		   }).

-record(rrd_xport,{vname,    % Variable name
		   legend    % Exported name
		  }).

%% Common for rrd_graph and rrd_xport

-record(rrd_def,{vname,      % Variable name
		 rrd,        % RRD filename
		 ds_name,    % Datasource name
		 cf          % Consolidation functions
		}).

-record(rrd_cdef,{vname,     % Variable name
		  rpn        % RPN expression
		 }).

-record(rrd_vdef,{vname,     % Variable name
		  rpn        % RPN expression
		 }).

%% rrd_graph
-record(rrd_graph,{file,start,stop,step,defs=[],cdefs=[],vdefs=[],graph=[],options=[]}).

-record(rrd_print,{vname,format}).

-record(rrd_gprint,{vname,format}).

-record(rrd_comment,{text}).

-record(rrd_vrule,{time,color,legend}).

-record(rrd_line,{width,vname,color,legend,stack}).

-record(rrd_area,{vname,color,legend,stack}).

-record(rrd_tick,{vname,rrggbb,aa,fraction,legend}).

-record(rrd_shift,{vname,offset}).
