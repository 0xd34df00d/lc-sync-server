-module(syncserver).
-export([start/2,stop/1,init/1]).
-export([start_user_list/0]).
-import(db_interface,[db_access/1]).
-import(net_interface,[sendm/2,receivem/1,list2int/1,int2list/1]).
-behaviour(application).
-behaviour(supervisor).
-define(ok_msg(),sendm(get(socket),["OK"])).
-define(delta_prefix,"DELTA\x00\x00\x00").
-define(delta_id_length,4).
-define(delta_zero_id,[0,0,0,0]).

% сообщения об ошибках и их коды.
-define(err_unknown_command,
	["ERR",[0,0,0,0],"Unknown command"]).
-define(err_user_registered,
	["ERR",[0,0,0,1],"User is registered already"]).
-define(err_user_not_registered,
	["ERR",[0,0,0,2],"User is not registered"]).
-define(err_password,
	["ERR",[0,0,0,3],"Wrong password"]).
-define(err_already_connected,
	["ERR",[0,0,0,4],"Already connected"]).
-define(err_odd_parameters,
	["ERR",[0,0,0,5],"Odd number of filter parameters"]).
-define(err_wrong_delta_id,
	["ERR",[0,0,0,6],"Wrong delta`s Id"]).

% коды успеха при работе с дельтами
-define(scGeneralSuccess,[0,0,0,0]).
-define(scMaxDeltaIDReceived,[0,0,0,1]).
-define(scDeltasReceived,[0,0,0,2]).

% TODO: рефакторинг.

start(_Type, StartArgs)->
	supervisor:start_link(?MODULE,StartArgs).

stop(_State)->
	ok.

init(Args) ->
	{ok,{{one_for_one, 3, 10},[
		{user_list,
			{syncserver,start_user_list,[]},
			transient,brutal_kill,worker,[]},
		{connection_handler_pool,
			% может {global,connection_handler_pool} ?
			{supervisor,start_link,[{local,connection_handlers},net_interface,Args]},
			permanent,infinity,supervisor,[net_interface]},
		{connection_receiver,
			{net_interface,server_start,[fun login/1]},
			permanent,brutal_kill,worker,[]}
	]}}.

start_user_list()->
	{ok,spawn_link(
		fun()->
			register(user_list,self()),
			user_list([])
		end)}.

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
		["PUTDELTA",Key,Id,Delta|DS]->
			put_deltas(Key,Id,[Delta|DS]),
			running();
		["GETDELTA",Key,StartId]->
			get_delta(Key,StartId),
			running();
		["MAXDELTA",Key]->
			max_delta(Key),
			running();		
		% обработка неизвестной команды
		_ -> sendm(get(socket),?err_unknown_command),
			running()
	end.

register_user(Socket,User,Password)->
	case db_access({user_exists,User}) of
		true->
			sendm(Socket,?err_user_registered);
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
	if	NotExists -> sendm(S,?err_user_not_registered);
		OrWrongPassword -> sendm(S,?err_password);
		OrConnected -> sendm(S,?err_already_connected);
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
			sendm(get(socket),?err_odd_parameters);
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
			sendm(get(socket),?err_odd_parameters);
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

put_deltas(Key,StartId,Deltas)->
	AllIds=lists:map(fun select_delta_id/1,all_deltas(Key)),
	io:format("put_delta.allIds: ~p~n",[AllIds]),
	% по идее допустимо сравнение id'ов без преобразования в int().
	case lists:all(fun(X)-> X<StartId end,AllIds) of
		true->
			lists:foreach(
				fun(V)-> 
					db_access({put,get(user),Key,V})
				end,form_deltas(StartId,Deltas)),
			sendm(get(socket),?scGeneralSuccess);
		_->
			sendm(get(socket),?err_wrong_delta_id)
	end.

form_deltas(StartId,Deltas)->
	IdInt=list2int(StartId),
	IDs=[int2list(I)||I<-lists:seq(IdInt,IdInt+length(Deltas)-1)],
	[?delta_prefix++Id++Delta||{Id,Delta}<-lists:zip(IDs,Deltas)].
	
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
	sendm(get(socket),["OK",?scDeltasReceived|Deltas]).

all_deltas(Key)->get_filtered_records(Key,[1,?delta_prefix]).
select_delta_data(Delta)->
	lists:nthtail(length(?delta_prefix)+?delta_id_length,Delta).
select_delta_id(Delta)->
	lists:sublist(Delta,1+length(?delta_prefix),?delta_id_length).

max_delta(Key)->
	AllIds=lists:map(fun select_delta_id/1,all_deltas(Key)),
	R = if AllIds =/= [] -> lists:max(AllIds); true -> ?delta_zero_id end,
	sendm(get(socket),["OK",?scMaxDeltaIDReceived, R]).


