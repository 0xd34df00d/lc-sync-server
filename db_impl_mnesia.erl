-module(db_impl_mnesia).
-export([init/0,start/0,run/0]).
-include("records.hrl").
-define(PASSWORD_KEY,'PASSWORD').

% запуск БД; возвращает поток, работающий с БД.
start()->
	mnesia:start(),
	spawn(?MODULE,run,[]).
% первичная инициализация БД
init()-> 
	mnesia:create_shema([node()]).
	
% формат сообщений:
% {Отправитель,{действие,Пользователь,..<параметры>..}}
% отправитель _обязательно_ уведомляется о результате дествия.

% поток работы с БД работает в этой функции.
run()->
	receive	
		{Sender,{list,User}}->
			Sender!db_list(User),
			run();
		{Sender,{set,User,Key,Value}}->
			Sender!db_set(User,Key,Value),
			run();
		{Sender,{get,User,Key}}->
			Sender!db_get(User,Key),
			run();
		{Sender,{erase_key,User,Key}}->
			Sender!db_erase_key(User,Key),
			run();
		{Sender,{create_user,User,Password}}->
			Sender!create_user(User,Password),
			run();
		{Sender,{delete_user,User}}->
			Sender!delete_user(User),
			run();
		{Sender,{user_exists,User}}->
			Sender!lists:member(User,mnesia:table_info(tables)),
			run();
		{Sender,{get_password,User}}->
			Sender!get_password(User),
			run();
		{Sender,{set_password,User,Password}}->
			Sender!set_password(User,Password),
			run();
		shutdown->
			db_shutdown;
		E-> error
	end.

db_list(User)->
	mnesia:all_keys(User).

db_set(User,Key,Value)->
	mnesia:transaction(
		fun()-> 
			mnesia:write(User,{Key,Value},write) 
		end).

db_get(User,Key)->
	mnesia:read(User,Key). %будет ли работать без транкзакции?
%можно так:
%	T=self(),
%	mnesia:transaction(
%		fun()-> 
%			T!mnesia:read(User,Key)
%		end),
%	receive A->A end.

get_password(User)->
	[#db_value{data=Data}] = db_get(User,?PASSWORD_KEY), Data.

set_password(User,Password)->
	db_set(User,
		?PASSWORD_KEY, 
		#db_value{version=null,
			description="User password/key",
			data=Password}).
	
db_erase_key(User,Key)->
	mnesia:transaction(
		fun()-> 
			mnesia:delete(User,Key,write) 
		end).

%надо бы еще устанавливать пароль в одной транзакции.
create_user(User,Password)->
	mnesia:create_table(list_to_atom(User),
		[ {attributes, record_info(fields,db_value)} ]),
	set_password(User,Password).

delete_user(User)->
	mnesia:delete_table(list_to_atom(User)).


