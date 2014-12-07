%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%     Proxy statistic implementation using riak CRDT feature with
%%%     automatically, Execute create_riak_crdt.sh fristly
%%% @end
%%%-------------------------------------------------------------------
-module(nasoc_riak_stat_crdt).

-include("nasoc.hrl").

%% API
-export([add_client/1, inc_counter/4, list_clients/0, count/1]).

%%% Bucket where set of all clients is stored
-define(ALL_CLIENTS_BUCKET, <<"nasoc_app_crdt_all_clients_bkt">>).

%%% Bucket key where set of all clients is stored
-define(ALL_CLIENTS_KEY, <<"nasoc_app_crdt_all_clients_key">>).

%%% Key where stored set of all targets for client
-define(TARGETS_IN_CLIENT_BUCKET, <<"nasoc_app_crdt_client_to_target_map_bkt">>).

-define(MAP, <<"nasoc_crdt_maps">>).
-define(SET, <<"nasoc_crdt_sets">>).


%%%-----------------------------------------------------------------------------
%%% Statistic storage in riak based on CRDT features.
%%%
%%% BUCKET ->
%%%     KEY ->
%%%         TYPE:ELEM
%%%
%%% SCHEMA
%%% 
%%% Here stored set of all client ips which connected to socks proxy
%%% ?ALL_CLIENTS_BUCKET ->
%%%     ?ALL_CLIENTS_KEY ->
%%%          SET:client_ip()   
%%%
%%% ?TARGETS_IN_CLIENT_BUCKET ->
%%%     client_ip_bin() ->
%%%          MAP: key - target_ip_bin(), value - non_neg_integer() - number of bytes
%%%                                                   received from target
%%%-------------------------------------------------------------------------------

%%%===============================================================================
%%% API
%%%===============================================================================

%%--------------------------------------------------------------------------------
-spec add_client( ClientIp :: client_ip()) -> ok.
%%--------------------------------------------------------------------------------
%% @doc 
%%   Add client to list of clients. 
%%   raise exception if riak error happened (except not found)
%% @end
%%--------------------------------------------------------------------------------
add_client(ClientIp) ->
    Fun = fun(Pid) -> add_client_fun(Pid, ClientIp) end,
    safe_exec(Fun).


%%--------------------------------------------------------------------------------
-spec list_clients() -> {ok, [client_ip()]} | {error, notfound}.
%%--------------------------------------------------------------------------------
%% @doc 
%%   List call clients connected to proxy ever.
%%   Raise exception if riak error happened (except not found)
%% @end
%%--------------------------------------------------------------------------------
list_clients() ->
    Fun = fun(Pid) -> list_clients_fun(Pid) end,
    safe_exec(Fun).

%%--------------------------------------------------------------------------------
-spec inc_counter( ClientIp :: client_ip(), 
			  ClientPort :: client_port(),
			  TargetIp :: target_ip(),
			  IncTraff :: non_neg_integer()) 
		-> ok | {ok, term()} | {error, notfound}.
%%--------------------------------------------------------------------------------
%% @doc 
%%   Add target for client to riak. raise exception if riak error happened (except not found)
%% @end
%%--------------------------------------------------------------------------------
inc_counter(_ClientIp, _ClientPort, _TargetIp, 0) ->
    ok;

inc_counter(ClientIp, _ClientPort, TargetIp, IncTraff) ->
    Fun = fun(Pid) -> inc_counter_fun(Pid, ClientIp, TargetIp, IncTraff) end,
    safe_exec(Fun). 


%%--------------------------------------------------------------------------------
-spec count( ClientIp :: client_ip() ) 
-> {ok, [{target_ip(), non_neg_integer()}]} | {error, notfound}.
%%--------------------------------------------------------------------------------
%% @doc 
%%   Count incoming traffic for client. 
%%   Raise exception if riak error happened (except not found)
%% @end
%%--------------------------------------------------------------------------------
count(ClientIp) ->
    Fun = fun(Pid) -> count_fun(Pid, ClientIp) end,
    safe_exec(Fun).

%%%===============================================================================
%%% Internal functions
%%%===============================================================================

%% @hidden 
%% Execute fun in riak pool
safe_exec(Fun) ->
    case riakpool:execute(Fun) of 
	{ok, ok} -> 
	    ok;
	{ok, {ok, Result}} ->
	    {ok, Result};
	{ok, {error, notfound}} ->
	    {error, notfound};
	{ok, Something} ->
	    Stacktrace = erlang:get_stacktrace(),
	    ?ERROR("Riak error ~p ~p", [Something, Stacktrace]),
	    exit({error, Something});
	{error, Reason} ->
	    Stacktrace = erlang:get_stacktrace(),
	    ?ERROR("Riak error ~p ~p", [Reason, Stacktrace]),
	    exit({error, Reason})
    end.

%% @hidden
%% Add client to set all clients connected to proxy ever.
%% It uses riak CRDT set 
add_client_fun(Pid, ClientIp) ->
    BinClientIp = to_bin(ClientIp),
    Set = riakc_set:new(),
    Set1 = riakc_set:add_element(BinClientIp, Set),
    case riakc_pb_socket:update_type(Pid,
				     {?SET, ?ALL_CLIENTS_BUCKET},
				     ?ALL_CLIENTS_KEY, 
				     riakc_set:to_op(Set1)) of 
	ok ->
	    ok;
	{error, Reason} ->
	    ?ERROR("Riak error client ~p reason ~p",[ClientIp, Reason]),
	    exit({riak_error, Reason})
    end.

%% @hidden
%% Increment counter of bytes for target.
inc_counter_fun(Pid, ClientIp, TargetIp, IncTraff) ->
    Map  = riakc_map:new(),
    Map1 = riakc_map:update({to_bin(TargetIp), counter},
                        fun(R) -> riakc_counter:increment(IncTraff, R) end,
                        Map),
    case riakc_pb_socket:update_type(Pid,
				     {?MAP, ?TARGETS_IN_CLIENT_BUCKET},
				     to_bin(ClientIp),
				     riakc_map:to_op(Map1)) of 
	ok ->
	    ok;
	{error, Reason} ->
	    ?ERROR("Riak error client ~p reason ~p",[ClientIp, Reason]),
	    exit({riak_error, Reason})
    end.

%% @hidden
%% Return {target_ip(), non_neg_integer()} list. 
%% it is saved as CRDT map where Target IP converted to binary is key
%% and sum of all bytes, received from target, is value (counter)
count_fun(Pid, ClientIp) ->
    Result = riakc_pb_socket:fetch_type(Pid,
					{?MAP, ?TARGETS_IN_CLIENT_BUCKET},
					to_bin(ClientIp)),
    case Result of 
	{ok, Maps} ->
	    KeyValue = riakc_map:value(Maps),
	    {ok, [{binary_to_term(TargetIp), CountedBytes} || 
		     {{TargetIp, counter}, CountedBytes} <- KeyValue]};
	Error ->
	    Error
    end.
			   
%% @hidden
%% List all clients which used proxy ever.
%% Saved as CRDT set
list_clients_fun(Pid) ->
    Result = riakc_pb_socket:fetch_type(Pid,
					{?SET, ?ALL_CLIENTS_BUCKET},
					?ALL_CLIENTS_KEY),
    case Result of 
	{ok, Counter1} ->
	    OrdSets = riakc_set:value(Counter1),
	    {ok, [binary_to_term(BinIp) || 
		     BinIp <- ordsets:to_list(OrdSets)]};
	{error,{notfound,set}} ->
	    {error, notfound};
	Error ->
	    Error
   end.

to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> term_to_binary(Term).
