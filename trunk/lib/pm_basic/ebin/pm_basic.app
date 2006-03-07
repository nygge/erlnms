{application, pm_basic,
 [{description, "Basic Performance Management Functions"},
  {applications,[kernel,stdlib,mnesia,rrdtool]},
  {included_applications,[pm_rrdtool]},
  {mod, {pm_basic,[]}}
 ]}.