{application, pm_rrdtool,
 [{description, "Performance Management RRDTool Backend"},
  {applications,[kernel,stdlib,mnesia,pm_basic,rrdtool]},
  {mod, {pm_rrdtool,[]}}
 ]}.