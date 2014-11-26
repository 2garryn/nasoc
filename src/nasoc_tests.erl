-module(nasoc_tests).


-include_lib("eunit/include/eunit.hrl").

-include("protocol_def.hrl").

-compile(export_all).

-define(LOCALHOST_IP, {127,0,0,1}).

-define(LOCALHOST_IP_BIN, <<127,0,0,1>>).

%% Test simple connection procedure
success_connect_test() ->
    ?assertEqual(ok, application:start(nasoc)),
    {Port, ServPid} = start_server(),
    Socket = connect(1080),
    %% start neg-ns with no auth
    ?assertEqual(ok, send(Socket, <<?PROTO_VER5, 1, ?NO_AUTH>>)),
    ?assertEqual(<<?PROTO_VER5, ?NO_AUTH>>, receive_msg(Socket)),
    
    %% Send request on connection
    CommandBin = <<?PROTO_VER5, ?CMD_CONNECT, ?RSV, 
		   ?ATYPE_IPV4, ?LOCALHOST_IP_BIN/binary, Port:16>>,
    ?assertEqual(ok, send(Socket, CommandBin)),
    <<?PROTO_VER5:8, ?CMD_SUCCESS:8, ?RSV:8, ?ATYPE_IPV4:8, 
      Ip:4/binary, LocalPort:16>> = receive_msg(Socket),
    ?assertEqual(Ip, ?LOCALHOST_IP_BIN),
    ?assertEqual({error, eaddrinuse}, gen_tcp:listen(LocalPort, [])),

    %% test messages exchange
    ?assertEqual(ok, send(Socket, <<"ClientTest">>)),
    ?assertEqual(<<"ServerTest">>, receive_msg(Socket)),
    ?assertEqual(ok, application:stop(nasoc)),
    stop_server(ServPid).


%% test bind socket procedure
success_bind_test() ->   
    ?assertEqual(ok, application:start(nasoc)),
    {Port, ServPid} = start_server(),
    Socket = connect(1080),

    %% start neg-ns with no auth
    ?assertEqual(ok, send(Socket, <<?PROTO_VER5, 1, ?NO_AUTH>>)),
    ?assertEqual(<<?PROTO_VER5, ?NO_AUTH>>, receive_msg(Socket)),

    %% Send request on connection
    ConnectBin = <<?PROTO_VER5, ?CMD_CONNECT, ?RSV, 
		   ?ATYPE_IPV4, ?LOCALHOST_IP_BIN/binary, Port:16>>,
    ?assertEqual(ok, send(Socket, ConnectBin)),
    <<?PROTO_VER5:8, ?CMD_SUCCESS:8, ?RSV:8, ?ATYPE_IPV4:8, 
      Ip:4/binary, LocalPort:16>> = receive_msg(Socket),
    ?assertEqual(Ip, ?LOCALHOST_IP_BIN),
    ?assertEqual({error, eaddrinuse}, gen_tcp:listen(LocalPort, [])),
    ?assertEqual(ok, send(Socket, <<"ClientTest">>)),
    ?assertEqual(<<"ServerTest">>, receive_msg(Socket)),

    %% BIND PART. Open new connection
    BindSocket = connect(1080),
    ?assertEqual(ok, send(BindSocket, <<?PROTO_VER5, 1, ?NO_AUTH>>)),
    ?assertEqual(<<?PROTO_VER5, ?NO_AUTH>>, receive_msg(BindSocket)),
    
    %% Send bind request. SOCKS should listen port on external side
    BindPort = Port + 1,
    BindBin = <<?PROTO_VER5, ?CMD_BIND, ?RSV, 
		?ATYPE_IPV4, ?LOCALHOST_IP_BIN/binary, BindPort:16>>,
    ?assertEqual(ok, send(BindSocket, BindBin)),

    %% First reply from SOCKS. Listen socket opened success.
    <<?PROTO_VER5:8, ?CMD_SUCCESS:8, ?RSV:8, ?ATYPE_IPV4:8, 
      Ip:4/binary, BindPort:16>> = receive_msg(BindSocket),

    %% Send ip/port to test server. It should use them to connect
    ?assertEqual(ok, send(Socket, <<Ip/binary, BindPort:16>>)),

    %% Second reply from SOCKS. test server connected to SOCKS
    <<?PROTO_VER5:8, ?CMD_SUCCESS:8, ?RSV:8, ?ATYPE_IPV4:8, 
      _ServerIp:4/binary, _ServerConnPort:16>> = receive_msg(BindSocket),

    %% Test exchange with test server
    ?assertEqual(<<"ServerTest">>, receive_msg(BindSocket)),
    ?assertEqual(ok, send(BindSocket, <<"ClientTest">>)),
    ?assertEqual(<<"ServerTest">>, receive_msg(BindSocket)),

    ?assertEqual(ok, application:stop(nasoc)),
    stop_server(ServPid).
    

send(Socket, Bin) ->
    gen_tcp:send(Socket, Bin).

receive_msg(Socket) ->
    receive {tcp, Socket, Data} -> Data
    after 5000 -> {error, cant_receive_message}
    end.
	    
start_server() ->	     
    Self = self(),
    Child = spawn_link(?MODULE, start_server2, [Self]),
    receive 
	{port, Child, Port} -> {Port, Self}
    after 5000 -> {error, cant_start_test_server}
    end.

stop_server(Pid) ->
    Pid ! stop.

start_server2(Parent) ->
    {ok, ListenSocket} = gen_tcp:listen(0, [binary]),
    {ok, Port} = inet:port(ListenSocket),
    Parent ! {port, self(), Port},
    {ok, AcptSock} = gen_tcp:accept(ListenSocket, 5000),
    ?assertEqual(ok, gen_tcp:close(ListenSocket)),
    loop(AcptSock).

loop(AcptSock) ->
    receive
	{tcp, AcptSock, <<"ClientTest">>} ->
	    gen_tcp:send(AcptSock, <<"ServerTest">>),
	    loop(AcptSock);
	{tcp, AcptSock,  <<Ip:4/binary, BindPort:16>>} ->
	    ?assertEqual(?LOCALHOST_IP_BIN, Ip),
	    Socket = connect( BindPort ),
	    timer:sleep(1000),
	    gen_tcp:send(Socket, <<"ServerTest">>),
	    loop(Socket);
	stop ->
	    ?assertEqual(ok, gen_tcp:close(AcptSock))
    end.
	    
connect(Port) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active, true}, binary]),
    Socket.
    
