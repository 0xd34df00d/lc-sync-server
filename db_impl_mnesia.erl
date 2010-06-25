-module(db_impl_mnesia).
-export([init/0,start/0,run/0]).
-include("records.hrl").

% запуск БД; возвращает поток, работающий с БД.
start()->
	mnesia:start(),
	spawn(?MODULE,run,[]).
% первичная инициализация БД
init()-> 
	mnesia:create_shema([node()]).
	
% формат сообщений:
% {Отправитель,{действие,Пользователь,..<данные>..}}
% отправитель _обязательно_ уведомляется о результате дествия.

% поток работы с БД работает в этой функции.
run()->
	receive
		{Sender,{set,User,Key,Value}}->
			Sender!db_set(User,Key,Value),
			run();
		{Sender,{get,User,Key}}->
			Sender!db_get(User,Key),
			run();
		{Sender,{erase_key,User,Key}}->
			Sender!db_erase_key(User,Key),
			run();
		{Sender,{create_user,User}}->
			Sender!create_user(User),
			run();
		{Sender,{delete_user,User}}->
			Sender!delete_user(User),
			run();
		shutdown->
			db_shutdown;
		E-> error
	end.

db_set(User,Key,Value)->
	mnesia:transaction(
		fun()-> 
			mnesia:write(User,Key,write) 
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
	
db_erase_key(User,Key)->
	mnesia:transaction(
		fun()-> 
			mnesia:delete(User,Key,write) 
		end).

create_user(User)->
	mnesia:create_table(list_to_atom(User),
		[ {attributes, record_info(fields,db_value)} ]).

delete_user(User)->
	mnesia:delete_table(list_to_atom(User)).


