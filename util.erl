-module(util).
-compile(export_all).

make()->
	lists:map(fun c:c/1,
		[syncserver,net_interface,db_impl_mnesia,db_interface]).

start()->	syncserver:start().
stop()->	syncserver:stop().

create_db()->
	db_impl_mnesia:init().

