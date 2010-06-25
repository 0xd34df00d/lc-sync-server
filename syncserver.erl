-module(syncserver).
-export([start/0],stop/0]).

start()->
	start_net,start_db.
	
stop()->
	stop_net,stop_db.


