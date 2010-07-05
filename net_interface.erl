-module(net_interface).
-export([start/1,start/3,stop/0,server/2,send/2,recv/1,sendm/2,receivem/1,
	list2int/1,int2list/1]).
-import(lists,[map/2,flatten/1,sublist/3,split/2,nthtail/2]).
-define(port,1024).
-define(num_servers,8).
-define(read_timeout,infinity).

start(Callback)->
	start(Callback,?num_servers,?port).

start(Callback,Num,LPort) ->
	register(ls_stopper,spawn(
		fun()-> 
			case gen_tcp:listen(LPort,
					[list,	%данные в виде списков
					{active, false},	%использовать recv для чтения
					{packet,4}	%первые 4 байта - длина
					]) of
				{ok, ListenSock} ->
					start_servers(Callback,Num,ListenSock),
					{ok, Port} = inet:port(ListenSock),
					Port;
				{error,Reason} ->
					{error,Reason}
  			end,
			receive 
				_->stop 
			end 
		end)).

stop()->
	ls_stopper!stop.

start_servers(_,0,_) -> ok;
start_servers(Callback,Num,LS) ->
    spawn(?MODULE,server,[Callback,LS]),
    start_servers(Callback,Num-1,LS).

server(Callback,LS) ->
	io:format("~p accept..~n",[self()]),
    case gen_tcp:accept(LS) of
        {ok,S} ->
        	io:format("~p accepted ~p~n",[self(),S]),
			try 
				Callback(S) 
            catch 
            	throw:X -> { thrown, X};
				exit:X -> { exited, X};
				error:X -> { error, X}
			after
				gen_tcp:close(S),
				erase()
			end,
            server(Callback,LS);
        Other ->
            io:format("thread ~p exit: accept returned ~w~n",[self(),Other]),
            ok
    end.

send(S,Msg)->
	io:format("SEND thread:~p socket:~p~ndata:~p~n",[self(),S,Msg]),
	gen_tcp:send(S,Msg).

recv(S)->
% "The Length argument is only meaningful when the socket is in raw mode"
	{ok,Packet} = gen_tcp:recv(S,0,?read_timeout),
	io:format("RECV thread:~p socket:~p~ndata:~p~n",[self(),S,Packet]),
	Packet.

sendm(S,Msg)->
	io:format("SENDM thread:~p socket:~p~nmsg:~p~n",[self(),S,Msg]),
	send(S,marshal(Msg)).

receivem(S)->
	R=unmarshal(recv(S)),
	io:format("RECEIVEM thread:~p socket:~p~nmsg:~p~n",[self(),S,R]),
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

list2int(YS)->
	lists:foldl(fun(X,SM)-> SM * 16#100 + X rem 16#100 end,0,YS).

int2list(V)->
	[(V bsl Sh) band 255||Sh<-[24,16,8,0]].


