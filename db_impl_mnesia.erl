-module(db_impl_mnesia).
-export([init/0,start/1,stop/0,db_access/1]).
-define(PASSWORD_KEY,'PASSWORD').
-define(PASSWORD_TABLE,'PASSWORDS').

% запуск БД; возвращает поток, работающий с БД.
start(_)->
	mnesia:start().

% первичная инициализация БД
init()-> 
	mnesia:create_schema([node()]).

% останов
stop()->
	mnesia:stop().

% доступ к БД.
% {действие,Пользователь,..<параметры>..}
db_access(P)->
	case P of	
		{list_keys,User}->				db_list_keys(list_to_atom(User));
		{put,User,Key,Value}->			db_put(list_to_atom(User),Key,Value);
		{set_uniq,User,Key,Value}->		db_set_uniq(list_to_atom(User),Key,Value);
		{get,User,Key}->				db_get(list_to_atom(User),Key);
		{erase_key,User,Key}->			db_erase_key(list_to_atom(User),Key);
		{erase_record,User,Key,Value}->	db_erase_record(list_to_atom(User),Key,Value);
		{create_user,User,Password}->	create_user(list_to_atom(User),Password);
		{delete_user,User}->			delete_user(list_to_atom(User));
		{user_exists,User}->			user_exists(list_to_atom(User));
		{list_users}->					list_users();
		{get_password,User}->			get_password(list_to_atom(User));
		{set_password,User,Password}->	set_password(list_to_atom(User),Password);
		E-> {unknown_command,E}
	end.

db_list_keys(User)->
	{atomic,V}=mnesia:transaction(fun()-> mnesia:all_keys(User)end),
	V.

db_put(User,Key,Value)->
	{atomic,V}=mnesia:transaction(
		fun()-> mnesia:write({User,Key,Value})end),
	V.

db_set_uniq(User,Key,Value)->
	{atomic,V}=mnesia:transaction(
		fun()-> 
			mnesia:delete({User,Key}),
			mnesia:write({User,Key,Value})
		end),
	V.

db_get(User,Key)->
	{atomic,V}=mnesia:transaction(
		fun()-> mnesia:read({User,Key})end),
	lists:map(fun({_,_,D})-> D end,V).

get_password(User)->
	[Data] = db_get(User,?PASSWORD_KEY), Data.

set_password(User,Password)->
	{atomic,V}=mnesia:transaction(
		fun()-> 
			mnesia:delete({User,?PASSWORD_KEY}),
			mnesia:write({User,?PASSWORD_KEY,Password})
		end),
	V.
	
db_erase_key(User,Key)->
	{atomic,V}=mnesia:transaction(
		fun()-> mnesia:delete(User,Key,write) end),
	V.

db_erase_record(User,Key,Data) ->
	{atomic,V}=mnesia:transaction(
		fun()-> mnesia:delete_object({User,Key,Data}) end),
	V.

%надо бы еще устанавливать пароль в одной транзакции.
create_user(User,Password)->
	{atomic,ok}=mnesia:create_table(User,	
		[	{type,bag},
			{disc_copies,[node()]}	]),
	set_password(User,Password).

delete_user(User)->
	mnesia:delete_table(list_to_atom(User)).

list_users()->
	mnesia:system_info(tables).

user_exists(User)->
	lists:member(User,mnesia:system_info(tables)).

