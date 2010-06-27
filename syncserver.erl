-module(syncserver).
-export([start/0,stop/0]).
-import(db_interface,[get_db/0]).
-include("records.hrl").

start()->
	db_interface:init(),
	net:start(fun login/1).

stop()->
	stop_net,
	get_db()!shutdown,
	init:stop(0).

login(Socket)->
	not_implemented.

running(Socket,User)->
	not_implemented.


