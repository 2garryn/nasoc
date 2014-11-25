%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%    Listener and acceptor for incoming requests
%%% @end
%%%-------------------------------------------------------------------
-module(nasoc_listener).

-behaviour(gen_server).

-include("nasoc.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { 
	  %% socket of listener
	  listener :: inet:socket(),
	  %% reference for async acceptor
	  acceptor :: reference()
	 }).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% init gen_server callback
init([]) ->
    Port       = nasoc_app:get_config(internal_port),
    IfAddress  = nasoc_app:get_config(internal_ip),
    Options = [binary,
    	       {ip, IfAddress},  {active, false}, 
    	       {reuseaddr, true},
    	       {packet, 0}],
    case gen_tcp:listen(Port, Options) of 
	{ok, ListenSocket} ->
	    {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
	    {ok, #state{listener = ListenSocket,
		        acceptor = Ref}};
	{error, Reason} ->
	    ?ERROR("Start listener error: ~p",[Reason]),
	    {stop, {listen_error, Reason}}
    end.

%% call callback. NOT USED
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% cast callback. NOT USED
handle_cast(_Request, State) ->
    {noreply, State}.

%% some magic used. When new client is connect we get async accept
%% message.
handle_info({inet_async, ListenSocket, Ref, {ok, AcptSocket}}, 
	    #state{listener = ListenSocket, acceptor = Ref} = State) ->
    case catch proceed_accept(ListenSocket, AcptSocket) of 
	{ok, NewAcptRef} ->
	    {noreply, State#state{acceptor = NewAcptRef}};
	{'EXIT', Reason} ->
	    ?ERROR("Accept failed with ~p",[Reason]),
	    {stop, Reason, State}
    end;

%% Error during accept
handle_info({inet_async, _ListenSocket, _Ref, Error}, State) ->
    ?ERROR("Accept async got error: ~p",[Error]),
    {stop, Error, State};

handle_info(Message, State) ->
    ?WARNING("Get wrong message ~p ", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% accept incomning connection and start connection handler
proceed_accept(ListenSocket, AcptSocket) ->
    set_accept_opts(ListenSocket, AcptSocket),
    {ok, {Address, Port}} = inet:peername(AcptSocket),
    ?INFO("New client ~p connected ip: ~p src port: ~p", [AcptSocket, Address, Port]),
    %% start connection handler
    nasoc_conn_handler:start(AcptSocket),    
    case prim_inet:async_accept(ListenSocket, -1) of
	{ok, NewRef} -> 
	    {ok, NewRef};
	{error, Reason1} -> 
	    ?ERROR("Async accept  ~p error: ~p",[ListenSocket, Reason1]),
	    exit({async_accept_renew, Reason1})
    end.

%% magic from gen_tcp and prim_inet to make accept async
set_accept_opts(L, S) ->
    case prim_inet:getopts(L, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case prim_inet:setopts(S, Opts) of
		ok -> 
		    inet_db:register_socket(S, inet_tcp),
		    {ok, S};
		Error -> 
		    prim_inet:close(S), 
		    ?ERROR("Cannot set options after accept. Error: ~p", [Error]),
		    erlang:exit({set_accept_opts, Error})
	    end;
	Error ->
	    prim_inet:close(S), 
	    ?ERROR("Cannot set options after accept. Error: ~p", [Error]),
	    erlang:exit({set_accept_opts, Error})
    end.
