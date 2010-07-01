-module(syncserver).
-export([start/0,stop/0]).
-import(db_interface,[db_access/1]).
-import(net_interface,[sendm/2,receivem/1]).
-define(ok_msg(),sendm(get(socket),["OK"])).

start()->
	db_interface:start(),
	register(user_list,spawn(fun ()-> user_list([]) end)),
	net_interface:start(fun login/1).

stop()->
	stop_net,
	user_list!stop,
	db_interface:stop(),
	init:stop(0).

% реализация мьютекса для каждого имени пользователя
user_list(US)->
	receive
		stop -> stop;
		{release,User}->
			user_list(lists:delete(User,US));
		{Sender,{check_add,User}}->
			user_list(
				case Sender!lists:member(User,US) of
					true->US;
					false->[User|US]
				end);
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

	
running()->
	case net_interface:receivem(get(socket)) of
		["LISTKEYS"]->
			list_keys(),
			running();
		["ERASEKEY",Key]->
			erase_key(Key),
			running();
		["ERASERECORD",Key,Value]->
			erase_record(Key,Value),
			running();
		["PUT",Key,Value]->
			put_value(Key,Value),
			running();
		["SETUNIQ",Key,Value]->
			set_value_uniq(Key,Value),
			running();
		["GET",Key]->
			get_value(Key),
			running();
		["SETPASSWORD",Password]->
			set_password(Password),
			running();
		["UNREGISTER"]->
			unregister_user();
		["DISCONNECT"]->
			disconnect(get(socket));
		_ -> sendm(get(socket),["ERR","Unknown command"])
	end.

register_user(Socket,User,Password)->
	case db_access({user_exists,User}) of
		true->
			sendm(Socket,["ERR","User is registered already"]);
		false ->
			db_access({create_user,User,Password}),
			sendm(Socket,["OK"])
	end.

unregister_user()->
	db_access({delete_user,get(user)}),
	?ok_msg().

do_login(S,User,Password)->
	NotExists = not db_access({user_exists,User}),
	OrWrongPassword = NotExists orelse Password =/= db_access({get_password,User}),
	OrConnected = OrWrongPassword orelse lib:sendw(user_list,{check_add,User}),
	if	NotExists -> sendm(S,["ERR","User is not registered"]);
		OrWrongPassword -> sendm(S,["ERR","Wrong password"]);
		OrConnected -> sendm(S,["ERR","Already connected"]);
		true ->
			sendm(S,["OK"]),
			try
				put(socket,S),
				put(user,User),
				running()
			after 
				user_list!{release,User},
				erase(socket),
				erase(user)
			end
	end.

disconnect(S) -> 
	sendm(S,["OK"]).

list_keys()->
	sendm(get(socket),["OK"|db_access({list_keys,get(user)})]).

put_value(Key,Value)->
	ok=db_access({put,get(user),Key,Value}),
	?ok_msg().

set_value_uniq(Key,Value)->
	ok=db_access({set_uniq,get(user),Key,Value}),
	?ok_msg().

get_value(Key)->
	sendm(get(socket),["OK"|db_access({get,get(user),Key})]).

erase_key(Key)->
	db_access({erase_key,get(user),Key}),
	?ok_msg().

erase_record(Key,Value)->
	db_access({erase_record,get(user),Key,Value}),
	?ok_msg().

set_password(Password)->
	db_access({set_password,get(user),Password}),
	?ok_msg().

