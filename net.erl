-module(net).
-export([start/1,start/3,server/2,send/2,recv/1]).
-define(port,1024).
-define(num_servers,8).

start(Callback)->start(Callback,?num_servers,?port).

start(Callback,Num,LPort) ->
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
    end.

start_servers(_,0,_) ->
    ok;

start_servers(Callback,Num,LS) ->
    spawn(?MODULE,server,[Callback,LS]),
    start_servers(Callback,Num-1,LS).

server(Callback,LS) ->
	io:format("~p accept..~n",[self()]),
    case gen_tcp:accept(LS) of
        {ok,S} ->
        	io:format("~p accepted ~p~n",[self(),S]),
            Callback(S),
            server(Callback,LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

send(S,Msg)->
	gen_tcp:send(S,Msg).

recv(S)->
% "The Length argument is only meaningful when the socket is in raw mode"
	{ok,Packet} = gen_tcp:recv(S,0),
	Packet.


