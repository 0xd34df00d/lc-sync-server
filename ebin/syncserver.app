{application, syncserver,
 [{description, "Syncronization Server" },
  {vsn, "0.1.0" },
  {modules, [syncserver,net_interface,db_impl_mnesia,db_interface]},
  {registered,[connection_handlers,user_list,connection_receiver]},
  {applications, [kernel,stdlib,mnesia,sasl]},
  {mod, {syncserver,[]}},
  {env,[{port,1024}]}
 ]}.

