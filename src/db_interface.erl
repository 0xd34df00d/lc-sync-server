-module(db_interface).
-export([start/1,stop/0,db_access/1]).
-define(IMPL_MODULE,db_impl_mnesia).

start(Params)->
	?IMPL_MODULE:start(Params).
	
stop()->
	?IMPL_MODULE:stop().

db_access(Q)->
% Модуль реализации может не допускать одновременных доступов к базе,
% и использовать отдельный поток. А может и допускать.
	error_logger:info_msg("db_interface:db_access(~p)~n",[Q]),
	?IMPL_MODULE:db_access(Q).

