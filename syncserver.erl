-module(syncserver).
-export([start/0,stop/0,start/2,stop/1]).
-import(db_interface,[db_access/1]).
-import(net_interface,[sendm/2,receivem/1,list2int/1,int2list/1]).
-behaviour(application).
-define(ok_msg(),sendm(get(socket),["OK"])).
-define(delta_prefix,"DELTA\x00\x00\x00").
-define(delta_id_length,4).
-define(delta_zero_id,[0,0,0,0]).

% TODO: нормальное поведение приложения.
start(Type, StartArgs) -> start().
stop(State) -> stop().

% TODO: рефакторинг.

start()->
	case whereis(user_list) of
		undefined ->
			register(user_list,spawn(fun ()-> user_list([]) end)),
			db_interface:start([]),
			net_interface:start(fun login/1);
		_ ->
			already_started
	end.

stop()->
	case whereis(user_list) of
		undefined -> 
			not_started;
		_->
			net_interface:stop(),
			user_list!stop,
			db_interface:stop()
	end.

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
		["GET",Key|CS]->
			get_values(Key,CS),
			running();
		["SELECT",Key,From,Len|CS]->
			select_values(Key,From,Len,CS),
			running();
		["SETPASSWORD",Password]->
			set_password(Password),
			running();
		["UNREGISTER"]->
			unregister_user();
		["DISCONNECT"]->
			disconnect(get(socket));
		% не-общие команды
		["PUTDELTA",Key,Id,Delta]->
			put_delta(Key,Id,Delta),
			running();
		["GETDELTA",Key,StartId]->
			get_delta(Key,StartId),
			running();
		["MAXDELTA",Key]->
			max_delta(Key),
			running();		
		% обработка неизвестной команды
		_ -> sendm(get(socket),["ERR","Unknown command"]),
			running()
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

get_values(Key,Conditions)->
	if 
		length(Conditions) rem 2 == 1 -> 
			sendm(get(socket),["ERR","Odd number of filter parameters"]);
		true->
			AllRecords=db_access({get,get(user),Key}),
			FilteredRecords=filter_records(AllRecords,Conditions),
			sendm(get(socket),["OK"|FilteredRecords])
	end.

filter_records(AllRecords,Conditions)->
	lists:foldl(
		fun({Offset,Pattern},Records)-> 
			lists:filter(
				fun(Record)->
					is_list(Record) andalso 
						Offset>0 andalso Offset=<length(Record) andalso 
						Pattern=:=lists:sublist(Record,Offset,length(Pattern))
				end,Records)
		end,AllRecords,
		%преобразование чисел из big endian
		[{if is_list(O)->list2int(O);true->O end,P}||{O,P}<-list_to_tuples(Conditions)]).

% TODO: протестировать это
select_values(Key,F,L,Conditions)->
	From=list2int(F),
	Len=list2int(L),
	if 
		length(Conditions) rem 2 == 1 -> 
			sendm(get(socket),["ERR","Odd number of filter parameters"]);
		true->
			AllRecords=db_access({get,get(user),Key}),
			FilteredRecords=filter_records(AllRecords,Conditions),
			ShrinkedRecords=lists:foldl(
				fun(R,Records)->
					case From>0 andalso From=<length(R) of
						true->
							[lists:sublist(R,From,Len)|Records];
						_->Records
					end
				end,[],FilteredRecords),
			sendm(get(socket),["OK"|ShrinkedRecords])
	end.

get_filtered_records(Key,Conditions)->
	AllRecords=db_access({get,get(user),Key}),
	filter_records(AllRecords,Conditions).

list_to_tuples([])->[];
list_to_tuples([X,Y|XS])->
	[{X,Y}|list_to_tuples(XS)].

erase_key(Key)->
	db_access({erase_key,get(user),Key}),
	?ok_msg().

erase_record(Key,Value)->
	db_access({erase_record,get(user),Key,Value}),
	?ok_msg().

set_password(Password)->
	db_access({set_password,get(user),Password}),
	?ok_msg().
	
% не-общие команды

put_delta(Key,Id,Delta)->
	AllIds=lists:map(fun select_delta_id/1,all_deltas(Key)),
	io:format("put_delta.allIds: ~p~n",[AllIds]),
	% по идее допустимо сравнение id'ов без преобразования в int().
	case lists:all(fun(X)-> X<Id end,AllIds) of
		true->
			D=db_access({put,get(user),Key,?delta_prefix++Id++Delta}),
			?ok_msg();
		_->
			sendm(get(socket),["ERR","Wrong Id"])
	end.

get_delta(Key,StartId)->
	All=get_filtered_records(Key,[1,?delta_prefix]),
	Filtered=lists:filter(
			fun(X)->
				select_delta_id(X)>StartId
			end,All),
	Sorted=lists:sort(
		fun(A,B)->
			select_delta_id(A)=<select_delta_id(B)
		end,Filtered),
	Deltas=lists:map(fun select_delta_data/1,Sorted),
	sendm(get(socket),["OK"|Deltas]).

all_deltas(Key)->get_filtered_records(Key,[1,?delta_prefix]).
select_delta_data(Delta)->
	lists:nthtail(length(?delta_prefix)+?delta_id_length,Delta).
select_delta_id(Delta)->
	lists:sublist(Delta,1+length(?delta_prefix),?delta_id_length).

max_delta(Key)->
	AllIds=lists:map(fun select_delta_id/1,all_deltas(Key)),
	R = if AllIds =/= [] -> lists:max(AllIds); true -> ?delta_zero_id end,
	sendm(get(socket),["OK", R]).


