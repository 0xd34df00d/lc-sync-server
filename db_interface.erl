-module(db_interface).
-export([init/0,shutdown/0,get_db/0,work/0,db_access/1]).

init()->
	W=register(db_interface_thread,spawn(?MODULE,work,[])),
	%io:format("~p~n",[W]),
	db_interface_thread!start.
	
shutdown()->
	db_interface_thread!stop.
	
work()->
	receive
		start ->
			case get(th) of 
				undefined ->
					% имя модуля здесь указывает на реализацию работы с БД.
					R=db_impl_mnesia:start(),
					put(th,R);
				A -> already_started
			end,
			work();
		stop ->
			get(th)!shutdown,
			unregister(db_interface_thread);
		{T,get_proc}->
			T!get(th),
			work();
		A -> error
	end.

%% возвращает поток, работающий с db
get_db()->
	lib:sendw(db_interface_thread,get_proc).

db_access(Q)->
	lib:sendw(get_db(),Q).

