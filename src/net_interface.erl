-module(net_interface).
-export([send/2,recv/1,sendm/2,receivem/1,list2int/1,int2list/1]).
-export([handler_thread/2,server_start/1,init/1]).
-import(lists,[map/2,flatten/1,sublist/3,split/2,nthtail/2]).
-define(read_timeout,infinity).

server_start(Callback)->
	{ok,spawn_link(fun()->
		register(connection_receiver,self()),
		error_logger:info_msg("Connection receiver: started in ~p~n",[self()]),
		{ok,Port}=application:get_env(port),
		error_logger:info_msg("Connection receiver: listening port is ~p~n",[Port]),
		error_logger:info_report("Connection receiver: creating ListenSocket.."),
		{ok,ListenSocket}=gen_tcp:listen(Port,
			[list,	%данные в виде списков
			{active, false},	%использовать recv для чтения
			{packet,4}	%первые 4 байта - длина
			]),
		connection_receiver(Callback,ListenSocket)
	end)}.

connection_receiver(Callback,ListenSocket)->
	error_logger:info_report("Connection receiver: accepting.."),
	{ok,S}=gen_tcp:accept(ListenSocket),
	error_logger:info_msg("Connection receiver: accepted socket ~p~n",[S]),
	supervisor:start_child(connection_handlers,child_spec_handler(Callback,S)),
	connection_receiver(Callback,ListenSocket).

init(_Args)->
	{ok,{{one_for_one, 3, 10},[]}}.

child_spec_handler(Callback,Socket)->
	{{net_thread,make_ref()},{net_interface,handler_thread,[Callback,Socket]},temporary,2,worker,[]}.

handler_thread(Callback,Socket)->
	{ok,spawn_link(fun()->
		error_logger:info_msg("Handler: ~p started~n",[self()]),
		try
			Callback(Socket)
		catch
			throw:X ->error_logger:error_msg("Handler: ~p throw:~p~n",[self(),X]);
			exit:X -> error_logger:error_msg("Handler: ~p exit:~p~n",[self(),X]);
			error:X ->error_logger:error_msg("Handler: ~p error:~p~n",[self(),X])
		after
			gen_tcp:close(Socket)
		end,
		error_logger:info_msg("Handler: ~p finish~n",[self()])
		end)}.

send(S,Msg)->
	error_logger:info_msg("SEND thread:~p socket:~p~ndata:~p~n",[self(),S,Msg]),
	gen_tcp:send(S,Msg).

recv(S)->
% "The Length argument is only meaningful when the socket is in raw mode"
	{ok,Packet} = gen_tcp:recv(S,0,?read_timeout),
	error_logger:info_msg("RECV thread:~p socket:~p~ndata:~p~n",[self(),S,Packet]),
	Packet.

sendm(S,Msg)->
	error_logger:info_msg("SENDM thread:~p socket:~p~nmsg:~p~n",[self(),S,Msg]),
	send(S,marshal(Msg)).

receivem(S)->
	R=unmarshal(recv(S)),
	error_logger:info_msg("RECEIVEM thread:~p socket:~p~nmsg:~p~n",[self(),S,R]),
	R.

marshal(List) ->
	L = int2list(length(List)),
	LS = map(fun (X)->int2list(length(X)) end,List),
	L ++ flatten(LS) ++ flatten(List).

unmarshal(List)->
	L = list2int(sublist(List,1,4)),
	LS = lengths(sublist(List,1+4,4*L)),
	extractLists(LS,nthtail(4*(L+1),List)).

extractLists([],[])->[];
extractLists([L|LS],XS)->
	{X,Rest}=split(L,XS),
	[X|extractLists(LS,Rest)].

lengths([])->[];
lengths(XS)->
	{V,Rest}=split(4,XS),
	[list2int(V)|lengths(Rest)].

list2int(L)->
	<<I:32/integer>> =list_to_binary(L), I.
	
int2list(I)->
	binary_to_list(<<I:32/integer>>).

