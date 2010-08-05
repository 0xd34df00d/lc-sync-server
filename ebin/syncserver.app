{application, syncserver,
 [{description, "Syncronization Server" },
  {vsn, "0.1.0" },
  {modules, [syncserver,net_interface,db_impl_mnesia,db_interface]},
  {registered,[net_stopper,user_list]},
  {applications, [kernel,stdlib,mnesia]},
  {mod, {syncserver,[]}},
  {start_phases, []}
 ]}.

