%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%     Riak handler and siblings resolver for statistic.
%%% TODO:
%%%     add prefix to nasoc buckets.
%%% @end
%%%-------------------------------------------------------------------
-module(nasoc_riak_stat).

-include("nasoc.hrl").

%% API
-export([add_client/1, inc_counter/4,
	count/1, list_clients/0]).

%%% Bucket where set of all clients is stored
-define(ALL_CLIENTS_BUCKET, <<"nasoc_app_all_clients_bkt">>).

%%% Bucket key where set of all clients is stored
-define(ALL_CLIENTS_KEY, <<"nasoc_app_all_clients_ip_key">>).

%%% Key where stored set of all targets for client
-define(TARGETS_IN_CLIENT_BUCKET, <<"nasoc_app_targets_keys_key">>).

%%% Prefix for client ip bucket
-define(NASOC_BUCKET_PREFIX, <<"nasoc_app_client_">>).

%% R + W > N - strong consistency
-define(N_VAL, 5).

-define(R, 3).

-define(W, 3).

-define(DW, 1).

%%%-------------------------------------------------------------------
%%%  RIAK SCHEMA definition
%%%  bucket ->
%%%      key ->
%%%          content
%%% ------------------------------------------------------------------
%%%   
%%%  All clients IPs which were connected with success. handshake
%%%  nasoc_all_clients_bkt ->  
%%%      nasoc_all_clients_ip_key ->  
%%%          gb_sets:set(client_ip()) - converted to binary
%%% ------------------------------------------------------------------
%%% 
%%% ?NASOC_BUCKET_PREFIX_client_ip_bin() ->
%%%     nasoc_targets_keys_key ->
%%%        gb_sets:set(target_ip()) - converted to binary
%%%     target_ip_bin() ->
%%%        gb_trees:tree(client_port(), pos_integer()) - converted to binary
%%%
%%%-------------------------------------------------------------------

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

inc_counter(ClientIp, ClientPort, TargetIp, IncTraff) ->
    Fun = fun(Pid) -> 
		  inc_counter_fun(Pid, ClientIp, ClientPort, 
					 TargetIp, IncTraff) 
	  end,
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
%% Try to get set of all clients connected ever
%% Add new client to this set. 
%% In case of siblings we can merge many sets without incosistency
add_client_fun(Pid, ClientIp) ->
    case riak_get(Pid, ?ALL_CLIENTS_BUCKET, ?ALL_CLIENTS_KEY) of 
	{ok, ValueObj} ->
	    add_to_sets(Pid, ClientIp, ValueObj);
	{error, notfound} ->
	    create_set(Pid, ?ALL_CLIENTS_BUCKET, 
			 ?ALL_CLIENTS_KEY, ClientIp);
	{error, Reason}  ->
	    ?ERROR("[client ip ~p] Riak get error ~p close connection now!",
		   [ClientIp, Reason]),
	    exit({riak_error, ClientIp, Reason})
    end.


%% @hidden 
%% increment or create traffic for client ip <-> target ip pair
inc_counter_fun(Pid, ClientIp, ClientPort, TargetIp, IncTraff) ->
    BinClientIp = ip_to_bucket(ClientIp),
    add_target_to_client(Pid, BinClientIp, TargetIp),
    case riak_get(Pid, BinClientIp, to_bin(TargetIp)) of 
	{ok, ValueObj} ->
	    increment_traffic_counters(Pid, ValueObj, ClientPort, IncTraff);
	{error, notfound} ->
	    create_traffic_counters(Pid, BinClientIp, TargetIp, ClientPort, IncTraff);
	{error, Reason} ->
	    ?ERROR("[client ip ~p] Riak error get ~p close connection now!",
		   [ClientIp, Reason]),
	    exit({riak_error, ClientIp, Reason})
    end.


list_clients_fun(Pid) ->
    case riak_get(Pid, ?ALL_CLIENTS_BUCKET, ?ALL_CLIENTS_KEY) of 
	{ok, ValueObj} ->
	    Sets = values(ValueObj), %% get values of object. it is list anyway
	    MergedSet = merge_sets(Sets),
	    {ok, gb_sets:to_list(MergedSet)};
	Error ->
	    Error
    end.     
		
%% @hidden 
%% count traffic from all targets 
count_fun(Pid, ClientIp) ->
    BinClientIp = ip_to_bucket(ClientIp),
    case riak_get(Pid, BinClientIp, ?TARGETS_IN_CLIENT_BUCKET) of 
	{ok, ValueObj} ->
	    Sets = values(ValueObj), %% get values of object. it is list anyway
	    MergedSet = merge_sets(Sets),
	    CountedTraff = count_targets(Pid, BinClientIp, MergedSet),
	    {ok, CountedTraff};
	Error ->
	    Error
    end. 
	    

%% @hidden
%% Add target to client targets set
add_target_to_client(Pid, BinClientIp, TargetIp) ->
    case riak_get(Pid, BinClientIp, ?TARGETS_IN_CLIENT_BUCKET) of 
	{ok, ValueObj} ->
	    add_to_sets(Pid, TargetIp, ValueObj);
	{error,notfound} ->
	    create_set(Pid, BinClientIp, ?TARGETS_IN_CLIENT_BUCKET, TargetIp); 
	{error, Reason} ->
	    ?ERROR("[client ip ~p] Riak error get ~p close connection now!",
		   [Reason]),
	    exit({riak_error,  Reason})
    end.


%% @hidden
%% Create gb_trees:tree(client_port(), non_neg_integer())
%% Key of tree is client port number
%% Value of tree is number of bytes
create_traffic_counters(Pid, BinClientIp, TargetIp, ClientPort, IncTraff) ->
    ok = riakc_pb_socket:set_bucket(Pid, BinClientIp, 
				    [{n_val, ?N_VAL}, 
				     {allow_mult, true}]),
    Trees0 = gb_trees:empty(),
    Trees1 = gb_trees:insert(ClientPort, IncTraff, Trees0),
    Object = riakc_obj:new(BinClientIp, to_bin(TargetIp), 
			   to_bin(Trees1)),
    riak_put(Pid, Object).

%% @hidden
%% 1. Get all siblings from client_ip() -> target_ip()
%% 2. Try to merge siblings. if we have two same pairs {client_port(), non_neg_integer}
%%    i.e two different counters of traffic for one port in two different siblings,
%%    we take bigger. 
%% 3. insert new {client_port(), non_neg_integer()} or increment current. 
increment_traffic_counters(Pid, ValueObj, ClientPort, IncTraffic) ->
    BinGbTrees = values(ValueObj),
    GbTree = merge_traffic_counters(BinGbTrees),
    NewGbTree = 
      case gb_trees:lookup(ClientPort, GbTree) of 
	  none -> gb_trees:insert(ClientPort, IncTraffic, GbTree); 
	  {value, Traffic} -> gb_trees:update(ClientPort, Traffic + IncTraffic, GbTree)
      end,
    riak_update(Pid, ValueObj, NewGbTree).


%% @hidden
%% Merge traffic counters siblings in one 
merge_traffic_counters([BinGbTree]) ->
    binary_to_term(BinGbTree);

merge_traffic_counters(BinGbTrees) ->
    Merged = gb_trees:empty(),
    merge_traffic_counters(BinGbTrees, Merged).

merge_traffic_counters([BinGbTree|BinGbTrees], Merged) ->
    GbTree = binary_to_term(BinGbTree),
    NewMerged = merge_pairs(GbTree, Merged),
    merge_traffic_counters(BinGbTrees, NewMerged);

merge_traffic_counters([], Merged) ->
    Merged.


%% @hidden
%% Merge two gb_trees with incoming bytes to one
%% if we have pair with the same port but different bytes count, take bigger 
merge_pairs(GbTree, Merged) ->
    Iter = gb_trees:iterator(GbTree),
    NewIter = gb_trees:next(Iter),
    merge_pairs_iter(NewIter, Merged).

merge_pairs_iter(none, Merged) ->
    Merged;
merge_pairs_iter({Port, TrafCount, Iter}, Merged) ->
    case gb_trees:lookup(Port, Merged) of 
	none ->
	    NewMerged = gb_trees:insert(Port, TrafCount, Merged),
	    NewIter = gb_trees:next(Iter),
	    merge_pairs_iter(NewIter, NewMerged);
	{value, Value} when Value >= TrafCount ->
	    NewIter = gb_trees:next(Iter),
	    merge_pairs_iter(NewIter, Merged);
	{value, Value} ->
	    NewMerged = gb_trees:update(Port, Value, Merged),
	    NewIter = gb_trees:next(Iter),
	    merge_pairs_iter(NewIter, NewMerged)
    end.

%% @hidden
%% Helper. Create set and put to riak		    
create_set(Pid, Bucket, Key, FirstElem) ->
    ok = riakc_pb_socket:set_bucket(Pid, Bucket, 
				    [{n_val, ?N_VAL}, 
				     {allow_mult, true}]),
    Set0 = gb_sets:new(),
    Set1 = gb_sets:add(FirstElem, Set0),
    Object = riakc_obj:new(Bucket, Key, to_bin(Set1)),
    riak_put(Pid, Object).
	    
%% @hidden
%% Helper. Resolve sets siblings and put new to this element not existed
add_to_sets(Pid, Ip, ValueObj) ->
    BinSets = values(ValueObj),
    MergedSet = merge_sets(BinSets),
    case gb_sets:is_member(Ip, MergedSet) of 
	true -> 
	    ok;
	false ->
	    MergedSet1 = gb_sets:add(Ip, MergedSet),
	    riak_update(Pid, ValueObj, MergedSet1)
    end.
   
%% @hidden
%% Helper. Merge several sets to one. Just simple union
merge_sets([Value]) ->
    binary_to_term(Value);

merge_sets(Values) -> 
    TermSets = [binary_to_term(BinSet) || BinSet <- Values],
    gb_sets:union(TermSets).


count_targets(Pid, BinClientIp, TargetSet) ->
    F = 
	fun(TargetIp, Acc) -> 
		[count_target(Pid, BinClientIp, TargetIp)|Acc] 
	end,
    gb_sets:fold(F, [], TargetSet).

%% @hidden 
%% Count incoming bytes for each target for one client
%% Siblings are resolved here.
count_target(Pid, BinClientIp, TargetIp) ->
    case riak_get(Pid, BinClientIp, to_bin(TargetIp)) of
	{ok, ValueObj} ->
	    BinGbTrees = values(ValueObj),
	    Merged = merge_traffic_counters(BinGbTrees),
	    Sum = sum_ports_traffic(Merged),
	    {TargetIp, Sum};
	{error, notfound} ->
	    {TargetIp, 0};
	{error, Reason} ->
	    ?ERROR("Riak get  ~p error ~p", [Reason]),
	    exit({riak_error, Reason})
    end.
			
sum_ports_traffic(GbTree) ->
    CounterValues = gb_trees:values(GbTree),
    lists:sum(CounterValues).
		
	
%% @hidden
%% get list of values	
values(ValueObj) ->
    case riakc_obj:value_count(ValueObj) of 
	1 -> [riakc_obj:get_value(ValueObj)];
	_N -> riakc_obj:get_values(ValueObj)
    end.



riak_update(Pid, ValueObj, NewValue) ->
    NewObj = riakc_obj:update_value(ValueObj, to_bin(NewValue)),
    NewObj1 = riakc_obj:update_metadata(NewObj, dict:new()),
    riak_put(Pid, NewObj1).
	    
riak_put(Pid, Object) ->
   case catch riakc_pb_socket:put(Pid, Object, [{w, ?W}, {dw, ?DW}]) of
       ok -> 
	   ok;
       Error ->
	   ?ERROR("Riak put object ~p error ~p", [Object, Error]),
	   exit({riak_put_error, Error})
   end.

riak_get(Pid, Bucket, Key) ->
    riakc_pb_socket:get(Pid, Bucket, Key, [{r, ?R}]).


ip_to_bucket(ClientIp) ->
    BinClientIp = to_bin(ClientIp),
    <<?NASOC_BUCKET_PREFIX/binary, BinClientIp/binary>>.

to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> term_to_binary(Term).
    

