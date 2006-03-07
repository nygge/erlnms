{application, pm_basic,
 [{description, "Basic Performance Management Functions"},
  {applications,[kernel,stdlib,mnesia,rrdtool]},
  {mod, {pm_basic,[]}}
 ]}.