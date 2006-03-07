{application, pm_rdbms,
 [{description, "Performance Management RDBMS Backend"},
  {applications,[kernel,stdlib,mnesia,odbc,pm_basic]},
  {mod, {pm_rdbms,[]}}
 ]}.