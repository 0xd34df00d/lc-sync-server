-module(syncserver).
-export([start/0,stop/0]).
-import(db_interface,[db_access/1]).
-import(net_interface,[sendm/2,receivem/1]).
-include("records.hrl").
-define(ok_msg(S),sendm(S,["OK"])).

start()->
	db_interface:init(),
	register(user_list,spawn(fun ()-> user_list([]) end)),
	net_interface:start(fun login/1).

stop()->
	stop_net,
	user_list!stop,
	db_interface:get_db()!shutdown,
	init:stop(0).

user_list(US)->
	receive
		stop -> stop;
		{add,User}->
			V=lists:member(User,US),
			user_list(if V -> US ; true -> [User|US] end);
		{release,User}->
			user_list(lists:delete(User,US));
		{Sender,{check,User}}->
			Sender!lists:member(User,US),
			user_list(US);
		{Sender,get_all}-> 
			Sender!US,
			user_list(US)
	end.

login(Socket)->
	case net_interface:receivem(Socket) of
		["REGISTER",User,Password]->
			register_user(Socket,User,Password);
		["LOGIN",User,Password]->
			do_login(Socket,User,Password)
	end.

running(Socket,User)->
%	put(socket,Socket),put(user,User),
	case net_interface:receivem(Socket) of
		["LISTKEYS"]->
			list_keys(Socket,User),
			running(Socket,User);
		["DELETEKEY",Key]->
			del_key(Socket,User,Key),
			running(Socket,User);
		["SET",Key,Version,Value]->
			set_value(Socket,User,Key,Version,Value),
			running(Socket,User);
		["GET",Key]->
			get_value(Socket,User,Key),
			running(Socket,User);
		["SETDESCR",Key,Descr]-> 
			set_descr(Socket,User,Key,Descr),
			running(Socket,User);
		["GETDESCR",Key]-> 
			get_descr(Socket,User,Key),
			running(Socket,User);
		["KEYVERSION",Key]->
			key_version(Socket,User,Key),
			running(Socket,User);
		["SETPASSWORD",Password]->
			set_password(Socket,User,Password),
			running(Socket,User);
		["UNREGISTER"]->
			unregister_user(Socket,User);
		["DISCONNECT"]->
			disconnect(Socket)
	end.

register_user(Socket,User,Password)->
	case db_access({user_exists,User}) of
		true->
			sendm(Socket,["ERR","User is registered already"]);
		false ->
			db_access({create_user,User,Password}),
			?ok_msg(Socket),
			running(Socket,User)
	end.

unregister_user(S,User)->
	db_access({delete_user,User}),
	?ok_msg(S).

do_login(S,User,Password)->
	NotExists = false =:= db_access({user_exists,User}),
	WrongPassword = Password =/= db_access({get_password,User}),
	Connected = true =:= lib:sendw(user_list,{check,User}),
	if	NotExists -> sendm(S,["ERR","User is not registered"]);
		WrongPassword -> sendm(S,["ERR","Wrong password"]);
		Connected -> sendm(S,["ERR","Already connected"]);
		true ->
			?ok_msg(S),
			user_list!{add,User},
			try	
				running(S,User)
			after 
				user_list!{release,User}
			end
	end.

disconnect(S) -> 
	?ok_msg(S).

list_keys(S,User)->
	sendm(S,["OK"|db_access({list,User})]).

set_value(S,User,Key,Vers,Value)->
	[R]=db_access({get,User,Key}),
	db_access({set,User,{Key,R#db_value{version=Vers,data=Value}}}),
	?ok_msg(S).

get_value(S,User,Key)->
	sendm(S,["OK"|lists:map(
		fun(X)-> X#db_value.data end,
		db_access({get,User,Key}))]).

set_descr(S,User,Key,Descr)->
	RS=db_access({get,User,Key}),
	lists:foreach(
		fun(X)-> 
			db_access({set,User,{Key,X#db_value{description=Descr}}})
		end,RS),
	?ok_msg(S).

get_descr(S,User,Key)->
	sendm(S,["OK"|lists:map(
		fun(X)-> X#db_value.description end,
		db_access({get,User,Key}))]).

key_version(S,User,Key)->
	RS=db_access({get,User,Key}),
	case RS of
		[] -> 
			sendm(S,["ERR","Key not exists"]);
		V -> 
			KS=lists:sort(fun (A,B)->A>B end,
				lists:map(fun(X)-> X#db_value.version end, V)),
			sendm(S,["OK"|KS])
	end.

del_key(S,User,Key)->
	db_access({erase_key,User,Key}),
	?ok_msg(S).

set_password(S,User,Password)->
	db_access({set_password,User,Password}),
	?ok_msg(S).

