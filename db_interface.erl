-module(db_interface).
-export([start/1,stop/0,db_access/1]).
-define(IMPL_MODULE,db_mnesia_impl).

start(Params)->
	?IMPL_MODULE:start(Params).
	
stop()->
	?IMPL_MODULE:stop().

db_access(Q)->
% Модуль реализации может не допускать одновременных доступов к базе,
% и использовать отдельный поток. А может и допускать.
	?IMPL_MODULE:db_access(Q).

