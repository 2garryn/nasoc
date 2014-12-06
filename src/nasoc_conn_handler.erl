%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%    Handle incoming connections
%%%    Only one authentificatio method is supported: no auth
%%%    Two command supported: CONNECT and BIND
%%% @end
%%%-------------------------------------------------------------------
-module(nasoc_conn_handler).

-include("protocol_def.hrl").
-include("nasoc.hrl").

-export([start/1]).

%% timeout on connect from external server during BIND command
-define(BIND_TIMEOUT, 10000).

-record(state, {
	  %% socket Client <-> Nasoc
	  cli_socket          :: inet:socket(),
	  %% socket Nasoc <-> External Server
	  ext_socket          :: inet:socket(),
	  %% ip and port of client 
	  cli_ip_port         :: {inet:ip_address(), inet:port_number()},
	  %% ip and port assigned on external server (nasoc external side)
	  ext_ip_port         :: {inet:ip_address(), inet:port_number()},
	  %% ip and port of target. (external server side)
	  target_ip_port      :: {inet:ip_address(), inet:port_number()},

	  tr_counter          :: pid(),
	  %% listener pid
	  parent              :: reference()}).

%% connected - when new client is connected to nasoc and we wait
%%             list of authentification methods
%% await_cmd - authentification methods are got and processed,
%%             we wait command from client
%% await_msg - command from client is processed and we start proxying
-type fsm_state() :: connected | 
		     await_cmd | 
		     await_msg.

%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
-spec start( CliSocket :: inet:socket() ) -> pid().
%%%-------------------------------------------------------------------
%% @doc
%%   Start handler
%% @end
%%%-------------------------------------------------------------------
start(CliSocket) ->
    Self = self(),
    Pid = spawn(fun() -> init_loop(Self, CliSocket) end),
    gen_tcp:controlling_process(CliSocket, Pid),
    Pid ! set_ctrl,
    Pid.


%%%===================================================================
%%% INTERNAL LOOP
%%%===================================================================
init_loop(ParentPid, CliSocket) ->
    Ref = erlang:monitor(process, ParentPid),
    ExtIp  = nasoc_app:get_config(external_ip),
    State = #state{ parent = Ref, 
		    cli_socket = CliSocket, 
		    ext_ip_port = {ExtIp, undefined}},
    Fsm = connected,
    loop(Fsm, State).

%%%-------------------------------------------------------------------
-spec loop( Fsm :: fsm_state(), State :: #state{} ) -> any().
%%%-------------------------------------------------------------------
%% Main loop on getting messages
%%%-------------------------------------------------------------------
loop(Fsm, State = #state{parent = Parent, tr_counter = TrCounter}) ->
    receive 
	set_ctrl ->
	    CliSocket = State#state.cli_socket,
	    {ok, {Ip, Port} = IpPort} = inet:peername(CliSocket),
	    set_active(CliSocket, once),
	    {ok, PidTrCounter} = 
		nasoc_traffic_counter:start_counter(self(), Ip, Port),
	    _Ref = erlang:monitor(process, PidTrCounter),
	    loop(Fsm, State#state{cli_ip_port = IpPort, 
				  tr_counter = PidTrCounter});
	{'DOWN', Parent, process, _Object, _Reason} ->
	    exit(listener_stopped);
	{'DOWN', _TrRef, process, TrCounter, _Reason} ->
	    exit(counter_stopped);
	Message ->
	    {NewFsm, NewState} = process(Message, Fsm, State),
	    loop(NewFsm, NewState)
    end.

%% set socket mode
set_active(Socket, Active) ->
    inet:setopts(Socket,[{active, Active}]).

%% send message to socket and stop if it can't be done 
socket_send(Socket, Message) ->
    case gen_tcp:send(Socket, Message) of 
	ok -> 
	    ok;
	{error, Reason} ->
	    ?ERROR("Error to send message to ~p message length ~p reason ",
		   [{Socket, inet:peername(Socket)}, byte_size(Message), Reason]),
	    exit({socket_send_error, Reason})
    end.

%% helper to route messages
process({tcp, Socket, Data}, Fsm, State = #state{cli_socket = Socket}) ->
    client_msg(Fsm, Data, State);

process({tcp, Socket, Data}, Fsm, State = #state{ext_socket = Socket}) ->
    target_msg(Fsm, Data, State);

process({tcp_closed, Socket}, Fsm, State = #state{cli_socket = Socket}) ->
    client_close(Fsm, State);

process({tcp_closed, Socket}, Fsm, State = #state{ext_socket = Socket}) ->
    target_close(Fsm, State);

process({tcp_error, Socket, Reason}, Fsm, State = #state{cli_socket = Socket}) ->
    client_error(Fsm, Reason, State);

process({tcp_error, Socket, Reason}, Fsm, State = #state{ext_socket = Socket}) ->
    target_error(Fsm, Reason, State);

process(WrongMessage, Fsm, State) ->
    ?WARNING("Wrong message ~p", [WrongMessage]),
    {Fsm, State}.


%% @hidden
%% Get auth. message. Check methods we supported and send reply if we have one 
%% from client list
client_msg(connected, <<?PROTO_VER5:8, MethNumber:8, ReqMethods/binary>>, 
	   State = #state{cli_ip_port = CliPort, cli_socket = CliSocket}) ->
    ?INFO("[client ~p] starts neg-ns. Supported auth methods: ~p",
	  [CliPort, meth_to_atom(MethNumber, ReqMethods)]),
    case is_method_supported(?NO_AUTH, MethNumber, ReqMethods) of 
	true -> 
	    socket_send(CliSocket, <<?PROTO_VER5, ?NO_AUTH>>),	
	    set_active(CliSocket, once),
	    {await_cmd, State};
	false -> 
	    ?INFO("[client ~p] no acceptable methods for client. Close connection...", 
		  [CliPort]),
	    socket_send(CliSocket, <<?PROTO_VER5, ?NO_ACPT_METHODS>>),
	    exit(wrong_clients_methods)
    end;

%% @hidden
%% Get CONNECT request. Try to connect to target host
client_msg(await_cmd, <<?PROTO_VER5:8, ?CMD_CONNECT:8, ?RSV:8, AType:8, AddrPort/binary>>, 
	   State = #state{cli_ip_port = CliPort, cli_socket = CliSocket}) ->
    case parse_atype(AType, AddrPort) of 
	{ok, Addr, Port} ->
	    ?INFO("[client ~p] wants to connect to ~p of supported type ~p", 
		  [CliPort, {Addr, Port}, atype_to_atom(AType)]),
	    connect_to_target(Addr, Port, AType, State);
	{error, BinReply, Reason} ->
	    ?ERROR("[client ~p] wants to use UNsupported type ~p of address. " 
		   "Error: ~p close connection ",
		   [CliPort, atype_to_atom(AType), Reason]),
	    socket_send(CliSocket, <<?PROTO_VER5, BinReply, ?RSV, AType, AddrPort/binary>>),
	    exit(unsup_address)
    end;

%% @hidden
%% Get BIND request. Try to open port on external side and wait connect from ext.server
client_msg(await_cmd, <<?PROTO_VER5:8, ?CMD_BIND:8, ?RSV:8, AType:8, AddrPort/binary>>, 
	   State = #state{cli_ip_port = CliPort, cli_socket = CliSocket}) ->
    case parse_atype(AType, AddrPort) of 
	{ok, Addr, Port} ->
	    ?INFO("[client ~p] wants to bind on external ~p of supported type ~p", 
		  [CliPort, {Addr, Port}, atype_to_atom(AType)]),
	    bind_for_target(Addr, Port, AType, State);
	{error, BinReply, Reason} ->
	    ?ERROR("[client ~p] wants to use UNsupported type ~p of address during BIND. " 
		   "Error: ~p close connection ",
		   [CliPort, atype_to_atom(AType), Reason]),
	    socket_send(CliSocket, <<?PROTO_VER5, BinReply, ?RSV, AType, AddrPort/binary>>),
	    exit(unsup_address)
    end;

%% @hidden
%% Get unsoppurted commands. Reply with error and exit
client_msg(await_cmd, <<?PROTO_VER5:8, UnsupCmd:8, Tail/binary>>, 
	   #state{cli_ip_port = CliPort, cli_socket = CliSocket}) ->
    ?ERROR("[client ~p] wants to use UNsupported command ~p",
	   [CliPort, UnsupCmd]),
    socket_send(CliSocket, <<?PROTO_VER5, ?CMD_NOT_SUPPORTED, Tail/binary>>),
    exit(unsup_command);

client_msg(FsmState, Binary, State) when FsmState == await_cmd;
					 FsmState == connected ->
    ?ERROR("[client ~p] sends wrong message during neg-ns ~p",
	   [State#state.cli_ip_port, erlang:binary_part(Binary, 0, 20)]), 
    exit(wrong_neg_message);
    

%% @hidden
%% Get proxy message. send it to target
client_msg(await_msg, Binary, State) ->
    socket_send(State#state.ext_socket, Binary),
    {await_msg, State}.

%% @hidden
%% Get proxy message. send it to client
target_msg(await_msg, Binary, State = #state{tr_counter = TrCounter}) ->
    nasoc_traffic_counter:inc_counter(TrCounter, byte_size(Binary)),
    socket_send(State#state.cli_socket, Binary),
    {await_msg, State}.

    
%% @hidden
%% Something goes wrong. Close all sockets
client_close(_Fsm, _State) ->
    exit(normal).

%% @hidden
%% Something goes wrong. Close all sockets
target_close(_Fsm, _State) ->
    exit(normal).
	    
%% @hidden
%% Client TCP error. Stop process and close sockets automatically
client_error(_Fsm, Reason, #state{cli_ip_port = CliPort,
				  target_ip_port = TipPort}) ->
    ?ERROR("[client ~p] Client error ~p. Target: ~p", [CliPort, Reason, TipPort]),
    exit({client_error, Reason}).

%% @hidden
%% Target TCP error. Stop process and close sockets automatically
target_error(_Fsm, Reason, #state{cli_ip_port = CliPort,
				  target_ip_port = TipPort}) ->
    ?ERROR("[client ~p] Target error ~p. Target: ~p", [CliPort, Reason, TipPort]),
    exit({target_socket_error, Reason}).

%% @hidden
%% Check that we support client's methods
is_method_supported(Method, MethNumber, ReqMethods) 
  when MethNumber == byte_size(ReqMethods) ->
    MethodsTrunc = erlang:binary_part(ReqMethods, 0, MethNumber),
    lists:member(Method, binary_to_list(MethodsTrunc));

is_method_supported(_Method, _MethNumber, _ReqMethods) ->
    false.

	
%% @hidden
%% Parse address from binary to erlang terms
parse_atype(?ATYPE_IPV4, <<Ip1, Ip2, Ip3, Ip4, Port:16, _/binary>>) -> 
    {ok, {Ip1, Ip2, Ip3, Ip4}, Port};
parse_atype(?ATYPE_DOMAIN, <<DLength:8, DomainPort/binary>>) -> 
    <<Domain:DLength/binary, Port:16, _/binary>> = DomainPort,
    {ok, binary_to_list(Domain), Port};
parse_atype(AType, Bin) -> 
    {error, ?ATYPE_NOT_SUPPORTED, {not_supported, AType, Bin}}.
	    
%% @hidden
%% Connect to target which was requested by client
connect_to_target(Addr, Port, AType, State) ->	
    #state{ext_ip_port = {ExtIp, _ExtPort}} = State,
    Result = gen_tcp:connect(Addr, Port, [{ip, ExtIp}, {active, true}, binary]),
    process_connect(Result, Addr, Port, AType, State).

%% @hidden
%% Process connect result ok. Reply to client that all is ok and it can
%% start sending messages
process_connect({ok, ExtSocket}, Addr, Port, _AType, State) -> 
    {ok, {ExtIp, ExtPort}} = inet:sockname(ExtSocket),
    ?INFO("[client ~p] server connected to remote target ~p with bind ip/port ~p",
	  [State#state.cli_ip_port, {Addr, Port}, {ExtIp, ExtPort}]),
    NewState = State#state{ext_socket = ExtSocket,
			   ext_ip_port = {ExtIp, ExtPort},
			   target_ip_port = {Addr, Port}},
    send_cmd_reply(ExtIp, ExtPort, ?ATYPE_IPV4, ?CMD_SUCCESS, State#state.cli_socket),
    set_active(State#state.cli_socket, true),
    nasoc_traffic_counter:target(NewState#state.tr_counter, Addr),
    {await_msg, NewState};

%% @hidden
%% Connect ot target error. reply to client and exit
process_connect({error, Error}, Addr, Port, AType, State) -> 
    ?ERROR("[client ~p] Error connect to ~p reason ~p",
	   [State#state.cli_ip_port, {Addr, Port}, Error]),
    BinError = error_to_bin(Error),
    send_cmd_reply(Addr, Port, AType, BinError, State#state.cli_socket),
    exit({connect_to_target_error, Error}).


error_to_bin(enetunreach) -> ?NETWORK_UNREACH;
error_to_bin(ehostunreach) -> ?HOST_UNREACH;
error_to_bin(econnrefused) -> ?CONN_REFUSED;
error_to_bin(_Error) -> ?ANY_OTHER_ERROR.
    

address_to_binary(?ATYPE_IPV4, {Ip1, Ip2, Ip3, Ip4}) ->
    <<Ip1, Ip2, Ip3, Ip4>>;
address_to_binary(?ATYPE_DOMAIN, Domain) when is_list(Domain) ->
    Length = length(Domain),
    Binary = list_to_binary(Domain),
    <<Length, Binary/binary>>;
address_to_binary(_Atype, Addr) when is_binary(Addr) ->
    Addr.
    
meth_to_atom(MethNumber, ReqMethods) ->
    [meth_to_atom(M) || M <- binary_to_list(erlang:binary_part(ReqMethods, 0, MethNumber))]. 

meth_to_atom(?NO_AUTH) -> no_auth_required;
meth_to_atom(?GSSAPI) ->  gssapi;
meth_to_atom(?USER_PWD) -> user_password; 
meth_to_atom(Meth)  when (Meth >= ?START_IANA) and (Meth =< ?END_IANA) -> iana_assigned;
meth_to_atom(Meth) when (Meth >= ?START_RSVD_FOR_PRIVATE) and 
			(Meth =< ?END_RSVD_FOR_PRIVATE) -> reserved_for_private;
meth_to_atom(Unknown) -> {unknown, Unknown}.
    
			
atype_to_atom(?ATYPE_IPV4) -> ip_v4;
atype_to_atom(?ATYPE_IPV6) -> ip_v6;
atype_to_atom(?ATYPE_DOMAIN) -> domain.


%% @hidden
%% BIND command processing. Resolve domain and go further with resolved.
bind_for_target(Addr, Port, ?ATYPE_DOMAIN, 
		State = #state{ ext_ip_port = {ExtIp, _Port},
				cli_socket = CliSocket}) ->
    case inet:getaddr(Addr, inet) of 
	{ok, Ip} when Ip == ExtIp ->
	    bind_for_target(Ip, Port, ?ATYPE_IPV4, State);
	{ok, OtherIp} ->
	    ?ERROR("[client ~p] wants bind on not exist server ip ~p domainname ~p port ~p",
		   [State#state.cli_ip_port, OtherIp, Addr, Port]),
	    send_cmd_reply(Addr, Port, ?ATYPE_DOMAIN, ?ANY_OTHER_ERROR, CliSocket),
	    exit(bind_to_wrong_server);
	{error, Reason} ->
	    ?ERROR("[client ~p] wants bind on not resolved domainname ~p port ~p reason ~p",
		   [State#state.cli_ip_port, Addr, Port, Reason]),
	    send_cmd_reply(Addr, Port, ?ATYPE_DOMAIN, ?ANY_OTHER_ERROR, CliSocket),
	    exit(bind_to_unres_hostname)
    end;

%% @hidden
%% BIND command processing.
%% 1. Try to listen requested socket. 
%% 2. Send First BIND reply.
%% 3. Wait on accept fro ONE connection and close listening socket.
%% 4. Send Second BIND reply to client that external server is connect to open socket
bind_for_target(Addr, Port, ?ATYPE_IPV4, 
		State = #state{cli_socket = CliSocket}) ->
    {AssignedPort, ListenSocket} = set_listen(Addr, Port, State),
    send_cmd_reply(Addr, AssignedPort, ?ATYPE_IPV4, ?CMD_SUCCESS, CliSocket),
    case gen_tcp:accept(ListenSocket, ?BIND_TIMEOUT) of 
	{ok, ConSocket} ->
	    ok = gen_tcp:close(ListenSocket),
	    {ok, {RemAddr, RemPort}} = inet:peername(ConSocket),
	    send_cmd_reply(RemAddr, RemPort, ?ATYPE_IPV4, ?CMD_SUCCESS, CliSocket),
	    set_active(CliSocket, true),
	    NewState = State#state{ ext_socket = ConSocket, 
				    ext_ip_port = {Addr, AssignedPort}, 
				    target_ip_port = {RemAddr, RemPort}},
	    nasoc_traffic_counter:target(NewState#state.tr_counter, {RemAddr, RemPort}),
	    {await_msg, NewState};
	{error, timeout} ->
	    send_cmd_reply(Addr, Port, ?ATYPE_IPV4, 
			   ?TTL_EXPIRED, CliSocket),
	    exit(timeout);
	 {error, Reason} ->	
	    ?ERROR("[client ~p] Server can't accept incoming connection"
		   " to ~p port ~p due internal error ~p",
		   [State#state.cli_ip_port, Addr, Port, Reason]),
	    send_cmd_reply(Addr, Port, ?ATYPE_IPV4, 
			   ?INTERNAL_ERROR, CliSocket),
	    exit({bind_accept_error, Reason})
    end.


%% @hidden
%% Try to listen on requested port and Ip.
%% If requested port is in use, select random port.
set_listen(Addr, Port, State) ->
    case gen_tcp:listen(Port, [{ip, Addr}, binary, {backlog, 1}]) of 
	{ok, ListenSocket} ->
	    {Port, ListenSocket};
	{error,eaddrinuse} ->
	    set_listen(Addr, 0, State);
	{error, Reason} ->
	    ?ERROR("[client ~p] Socks can't bind address  ~p port ~p due internal error ~p",
		   [State#state.cli_ip_port, Addr, Port, Reason]),
	    send_cmd_reply(Addr, Port, ?ATYPE_IPV4, 
			   ?INTERNAL_ERROR, State#state.cli_socket),
	    exit({set_listen_error, Reason})
    end.

%% @hidden
%% Command result sender
send_cmd_reply(Addr, Port, Atype, Result, Socket) -> 
    BinAddr  = address_to_binary(Atype, Addr),
    BinReply = <<?PROTO_VER5, Result, ?RSV,
		 Atype, BinAddr/binary, Port:16>>,
    socket_send(Socket, BinReply).
