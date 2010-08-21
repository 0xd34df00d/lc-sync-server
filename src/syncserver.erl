-module(syncserver).
-export([start/2,stop/1,init/1]).
-export([start_user_list/0]).
-import(db_interface,[db_access/1]).
-import(net_interface,[list2int/1,int2list/1]).
-import(net_interface,[sendm/2,receivem/1]).
-behaviour(application).
-behaviour(supervisor).

-define(ok_text,"OK").
-define(err_text,"ERR").

-define(delta_prefix,"DELTA\x00\x00\x00").
-define(delta_id_length,4).
-define(delta_zero_id,[0,0,0,0]).

% сообщения об ошибках и их коды.
-define(err_unknown_command,
	[?err_text,[0,0,0,0],"Unknown command"]).
-define(err_user_registered,
	[?err_text,[0,0,0,1],"User is registered already"]).
-define(err_user_not_registered,
	[?err_text,[0,0,0,2],"User is not registered"]).
-define(err_password,
	[?err_text,[0,0,0,3],"Wrong password"]).
-define(err_already_connected,
	[?err_text,[0,0,0,4],"Already connected"]).
-define(err_odd_parameters,
	[?err_text,[0,0,0,5],"Odd number of filter parameters"]).
-define(err_wrong_delta_id,
	[?err_text,[0,0,0,6],"Wrong delta`s Id"]).

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
			% может {global,connection_handlers} ?
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
			error_logger:info_msg("User list: started in ~p~n",[self()]),
			user_list([])
		end)}.

% реализация мьютекса для каждого имени пользователя
user_list(US)->
	receive
		{release,User}->
			error_logger:info_msg("User list: release user ~p~n",[User]),
			user_list(lists:delete(User,US));
		{Sender,{check_add,User}}->
			user_list(
				case Sender!lists:member(User,US) of
					true->
						error_logger:info_msg("User list: check_add user ~p: already connected~n",[User]),
						US;
					false->
						error_logger:info_msg("User list: check_add user ~p: added to list~n",[User]),
						[User|US]
				end);
		M -> 
			error_logger:warning_msg("User list: unhandled message: ~p~n",[M]),
			user_list(US)
	end.

login(Socket)->
	error_logger:info_msg("Handler: ~p in LOGIN state~n",[self()]),
	case receivem(Socket) of
		["REGISTER",User,Password]->
			register_user(Socket,User,Password);
		["LOGIN",User,Password]->
			do_login(Socket,User,Password)
	end.

	
running()->
	error_logger:info_msg("Handler: ~p in RUNNING state~n",[self()]),
	case receivem(get(socket)) of
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
		_ -> send_message(?err_unknown_command),
			running()
	end.

register_user(Socket,User,Password)->
	error_logger:info_msg("Registration: trying to register user ~p..~n",[User]),
	case db_access({user_exists,User}) of
		true->
			error_logger:info_msg("Registration: user ~p was not registered~n",[User]),
			sendm(Socket,?err_user_registered);
		false ->
			db_access({create_user,User,Password}),
			error_logger:info_msg("Registration: user ~p was registered~n",[User]),
			sendm(Socket,[?ok_text])
	end.

unregister_user()->
	db_access({delete_user,User=get(user)}),
	error_logger:info_msg("Unregistration: user ~p was unregistered~n",[User]),
	ok_msg().

do_login(S,User,Password)->
	error_logger:info_msg("Login: trying to login as user ~p..~n",[User]),
	NotExists = not db_access({user_exists,User}),
	OrWrongPassword = NotExists orelse Password =/= db_access({get_password,User}),
	OrConnected = OrWrongPassword orelse lib:sendw(user_list,{check_add,User}),
	if	NotExists -> 
			error_logger:info_msg("Login: user ~p don`t exists~n",[User]),
			sendm(S,?err_user_not_registered);
		OrWrongPassword -> 
			error_logger:info_msg("Login: wrong password for user ~p~n",[User]),
			sendm(S,?err_password);
		OrConnected -> 
			error_logger:info_msg("Login: user ~p is already connected~n",[User]),
			sendm(S,?err_already_connected);
		true ->
			sendm(S,[?ok_text]),
			error_logger:info_msg("Login: ~p logined as user ~p~n",[self(),User]),
			try
				put(socket,S),
				put(user,User),
				running()
			after
				error_logger:info_msg("Login: user ~p disconnected~n",[User]),
				user_list!{release,User},
				erase(socket),
				erase(user)
			end
	end.

disconnect(S) -> 
	sendm(S,[?ok_text]).

list_keys()->
	send_message([?ok_text|db_access({list_keys,get(user)})]).

put_value(Key,Value)->
	ok=db_access({put,get(user),Key,Value}),
	ok_msg().

set_value_uniq(Key,Value)->
	ok=db_access({set_uniq,get(user),Key,Value}),
	ok_msg().

get_values(Key,Conditions)->
	if 
		length(Conditions) rem 2 == 1 -> 
			send_message(?err_odd_parameters);
		true->
			FilteredRecords=get_filtered_records(Key,Conditions),
			send_message([?ok_text|FilteredRecords])
	end.

% TODO: протестировать это
select_values(_,_,_,Conditions) when length(Conditions) rem 2 == 1 -> 
	send_message(?err_odd_parameters);
select_values(Key,F,L,Conditions) ->
	From=list2int(F),
	Len=list2int(L),
	FilteredRecords=get_filtered_records(Key,Conditions),
	ShrinkedRecords=shrink_records(From,Len,FilteredRecords),
	send_message([?ok_text|ShrinkedRecords]).

erase_key(Key)->
	db_access({erase_key,get(user),Key}),
	ok_msg().

erase_record(Key,Value)->
	db_access({erase_record,get(user),Key,Value}),
	ok_msg().

set_password(Password)->
	db_access({set_password,get(user),Password}),
	ok_msg().

get_all_records(Key)->
	db_access({get,get(user),Key}).

get_filtered_records(Key,Conditions)->
	AllRecords=get_all_records(Key),
	filter_records(AllRecords,Conditions).

filter_records(AllRecords,Conditions)->
	%преобразование чисел из big endian (на входе могут быть числа или списки)
	Patterns=[{if is_list(Offset)->list2int(Offset);true->Offset end,Pattern}
		|| {Offset,Pattern} <-list_to_tuples(Conditions)],
	lists:filter(
		fun(Record)->
			lists:all(
				fun(Condition)->
					contains_sublist(Record,Condition)
				end,Conditions)
		end,AllRecords).

contains_sublist(Record,{Offset,Sublist})->
	is_list(Record) andalso Offset>0 andalso Offset<=length(Sublist) andalso
		Sublist =:= lists:sublist(Record,Offset,length(Sublist)).

list_to_tuples([])->[];
list_to_tuples([X,Y|XS])->
	[{X,Y}|list_to_tuples(XS)].

shrink_records(From,Len,Records)->
	lists:foldl(
		fun(R,RS)->
			case From>0 andalso From=<length(R) of
				true->
					[lists:sublist(R,From,Len)|RS];
				_-> RS % если запись слишком коротка, то пропускаем её
			end
		end,[],Records).

% Отправка сообщения на сокет, который есть в словаре процесса.
send_message(Msg)->
	net_interface:sendm(get(socket),Msg).

% Чтение сообщения из сокета, который есть в словаре процесса.
receive_message()->
	net_interface:reveivem(get(socket)).

% Отправка сообщения об успехе операции.
ok_msg()->
	send_message([?ok_text]).
	
% не-общие команды

put_deltas(Key,StartId,Deltas)->
	MaxId=max_delta_id(all_deltas(Key)),
	error_logger:info_msg("put_deltas: ~p: key '~p': MaxId=~p StartId=~p",
		[self(),Key,MaxId,StartId]),
	% по идее допустимо сравнение id'ов без преобразования в int().
	if
		MaxId<StartId ->
			lists:foreach(
				fun(V)-> 
					ok=db_access({put,get(user),Key,V})
				end,form_deltas(StartId,Deltas)),
				error_logger:info_msg(
					"put_deltas: ~p: put ~p deltas with key '~p': success",
					[self(),length(Deltas),Key]),
			ok_msg();
		true->
			error_logger:info_msg("put_deltas: ~p: key '~p': wrong delta id",
				[self(),Key]),
			send_message(?err_wrong_delta_id)
	end.
	
get_delta(Key,StartId)->
	FilteredById=lists:filter(
			fun(X)->
				select_delta_id(X)>StartId
			end,all_deltas(Key)),
	Sorted=lists:sort(
		fun(A,B)->
			select_delta_id(A)=<select_delta_id(B)
		end,FilteredById),
	Deltas=lists:map(fun select_delta_data/1,Sorted),
	send_message([?ok_text|Deltas]).

max_delta(Key)->
	MaxId=max_delta_id(all_deltas(Key)),
	send_message([?ok_text, MaxId]).

all_deltas(Key)->
	get_filtered_records(Key,[1,?delta_prefix]).

select_delta_data(Delta)->
	lists:nthtail(length(?delta_prefix)+?delta_id_length,Delta).

select_delta_id(Delta)->
	lists:sublist(Delta,1+length(?delta_prefix),?delta_id_length).

form_deltas(StartId,Deltas)->
	IdInt=list2int(StartId),
	IDs=[int2list(I)||I<-lists:seq(IdInt,IdInt+length(Deltas)-1)],
	[?delta_prefix++Id++Delta||{Id,Delta}<-lists:zip(IDs,Deltas)].

max_delta_id(Deltas)->
	case lists:map(fun select_delta_id/1,Deltas) of
		[] -> 
			?delta_zero_id;
		AllIds -> 
			% по идее допустимо сравнение id'ов без преобразования в int().
			lists:max(AllIds)
	end.

