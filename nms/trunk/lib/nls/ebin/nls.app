{application, nls,
 [{description,"Simple National Language support"},
  {vsn,"0.1"},
  {modules,[initDB, mkTabs, nls, nls_server, sup_nls]},
  {applications,[kernel,stdlib]},
  {mod, {nls,[]}},
  {env,[]}
 ]}.
