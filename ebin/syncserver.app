{application, syncserver,
 [{description, "Syncronization Server" },
  {vsn, "0.1.0" },
  {modules, [syncserver,net_interface,db_impl_mnesia,db_interface]},
  {registered,[handler_pool,user_list]},
  {applications, [kernel,stdlib,mnesia]},
  {mod, {syncserver,[]}},
  {env,[{port,1024}]}
 ]}.

