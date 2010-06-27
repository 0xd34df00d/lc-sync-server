-module(syncserver).
-export([start/0,stop/0]).
-import(db_interface,[get_db/0]).
-import(net_interface,[sendm/1,receivem/1]).
-include("records.hrl").

start()->
	db_interface:init(),
	net_interface:start(fun login/1).

stop()->
	stop_net,
	get_db()!shutdown,
	init:stop(0).

login(Socket)->
	case net_interface:receivem(Socket) of
		["REGISTER",User,Password]->
			reg,login;
		["UNREGISTER",User,Password]->
			unreg;
		["LOGIN",User,Password]->
			login
	end.

running(Socket,User)->
	case net_interface:receivem(Socket) of
		["LISTKEYS"]->
			listkeys,
			running(Socket,User);
		["DELETEKEY",Key]->
			delkey,
			running(Socket,User);
		["SET",Key,Version,Value]->
			set,
			running(Socket,User);
		["GET",Key]->
			get,
			running(Socket,User);
		["SETDESCR",Key,Descr]->
			setd,
			running(Socket,User);
		["GETDESCR",Key]->
			getd,
			running(Socket,User);
		["KEYVERSION",Key]->
			keyvers,
			running(Socket,User);
		["DISCONNECT"]->
			disconnect
	end.


