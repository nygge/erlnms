{application, rrdtool,
 [{description,"RRDTool interface"},
  {applications,[kernel,stdlib]},
  {mod, {rrdtool_app,[]}},
  {env,[{no_rrdtool_workers,3}]}
 ]}.