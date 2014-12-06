%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%    Separate process to count traffic from target ip to client.
%%%    1. Connection handler should call client/2 when it gets 
%%%       control from acceptor
%%%    2. target2client/2  should be called on each incoming packet 
%%%       from target.
%%%    When connection handler ends this proccess get 'DOWN' message
%%%    and writers counted traffic to riak
%%%    It uses riak by default.
%%%    TODO: create behaviour to have custom backaneds, not only riak
%%%          create option for export_to_file/2 to export info about
%%%          all clients in one command
%%%    
%%% @end
%%%-------------------------------------------------------------------
-module(nasoc_traffic_counter).

-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("nasoc.hrl").


%% API
-export([start_link/1]).
-export([start_counter/3]).

-export([target/2, 
	 inc_counter/2,
	 export_to_file/2,
	 export/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(COUNTER, nasoc_riak_stat).

-record(state, {
	  %% monitor reference of connection hadnler
	  conn_ref :: reference(),
	  %% Ip of client connected to handler
	  cl_ip    :: inet:ip_address(),
	  %% Port of client which is connected ot targer
	  cl_port  :: inet:port_number(),
	  %% ip of target
	  tg_ip    :: inet:ip_address(),
	  %% traffic counter in bytes
	  inc_traffic :: non_neg_integer()
	 }).

%%%===============================================================================
%%% API
%%%===============================================================================
%%--------------------------------------------------------------------------------
-spec start_counter( ConnPid :: pid(), 
		     ClientIp :: client_ip(), 
		     ClientPort :: client_port()) -> {ok, pid()}.
%%--------------------------------------------------------------------------------
%% @doc 
%%   Start new counter. Should be called by conn. handler before proxy handshaking
%% @end
%%--------------------------------------------------------------------------------
start_counter(ConnPid, ClientIp, ClientPort) ->
    supervisor:start_child(nasoc_traffic_counter_sup, 
			   [[ConnPid, ClientIp, ClientPort]]).


%%--------------------------------------------------------------------------------
-spec start_link( Args :: [term()]) -> {ok, pid()}.
%%--------------------------------------------------------------------------------
%% @doc 
%%   Start of process. Executed by supervisor
%% @end
%%--------------------------------------------------------------------------------
start_link(Args) ->    
    gen_server:start_link(?MODULE, Args, []).


%%--------------------------------------------------------------------------------
-spec target( Pid :: pid(), Ip :: target_ip()) -> ok.
%%--------------------------------------------------------------------------------
%% @doc 
%%   Set target ip. Should be called after handshaking between proxy and client
%% @end
%%--------------------------------------------------------------------------------
target(Pid, Ip) ->
    gen_server:cast(Pid, {target, Ip}).


%%--------------------------------------------------------------------------------
-spec inc_counter( Pid :: pid(), MsgSize :: non_neg_integer()) -> ok.
%%--------------------------------------------------------------------------------
%% @doc 
%%   Increment counter of bytes. MsgSize should be amount of bytes
%% @end
%%--------------------------------------------------------------------------------
inc_counter(Pid, MsgSize) ->
    gen_server:cast(Pid, {inc_counter, MsgSize}).


%%--------------------------------------------------------------------------------
-spec export_to_file( ClientIp :: client_ip(), 
		      FileName :: file:filename()) -> ok | {error, Reason :: term()}.
%%--------------------------------------------------------------------------------
%% @doc 
%%     Save statistic about client ip to file.
%%     You will gen detalized bytes got from each target ip and summurized 
%%     incoming traffic. If something goes wrong, file will be cleaned up
%% @end
%%--------------------------------------------------------------------------------
export_to_file(ClientIp, FileName) ->
    case ?COUNTER:count(ClientIp) of 
	{ok, TrafficCounter} -> save_to_file(TrafficCounter, FileName);
	{error, _Reason} = Error -> Error
    end.

%%--------------------------------------------------------------------------------
-spec export( ClientIp :: client_ip()) -> 
    {client_ip(), [{target_ip(), non_neg_integer()}]} | {error, Reason :: term()}.
%%--------------------------------------------------------------------------------
%% @doc 
%%     Get traffic statistic as erlang structure.
%%     {ClientIp :: inet:ip_address(), 
%%          [ { TargetIp :: inet:ip_address(), Bytes :: non_neg_integer() }]}
%% @end
%%--------------------------------------------------------------------------------
export(ClientIp) ->
    ?COUNTER:count(ClientIp).

%%%===============================================================================
%%% GEN_SERVER CALLBACKS
%%%===============================================================================

%% @hidden
%% Init callback.
init([ConnPid, ClientIp, ClientPort]) ->
    Ref = erlang:monitor(process, ConnPid),
    {ok, #state{conn_ref = Ref, cl_ip = ClientIp, 
		cl_port = ClientPort, inc_traffic = 0}}.

%% @hidden
%% gen_server:call/2 callback. Not used
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @hidden
%% gen_server:cast/2 callback
handle_cast({target, Ip}, State = #state{cl_ip = ClIp}) ->
    NewState = State#state{tg_ip = Ip},
    ?COUNTER:add_target(ClIp, Ip),
    {noreply, NewState};

handle_cast({inc_counter, MsgSize}, State = #state{inc_traffic = IncTr}) ->
    NewState = State#state{inc_traffic = IncTr + MsgSize},
    {noreply, NewState}.

%% @hidden
%% Get message that connection handler if that closed.
handle_info({'DOWN', ConnRef, process, _Pid, _Reason}, 
	    State = #state{inc_traffic = IncTraff, 
			   conn_ref = ConnRef,
			   cl_ip = CliIp, 
			   cl_port = CliPort,
			   tg_ip = TgIp}) ->
    ?COUNTER:inc_counter(CliIp, CliPort, TgIp, IncTraff),
    {stop, normal, State};

%% @hidden
%% get wrong message
handle_info(Info, State) ->
    ?ERROR("Wrong message ~p",[ Info ]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%%  Open file and start writting
save_to_file({ClientIp, TrafficCounters}, FileName) ->
    case file:open(FileName, [exclusive, write]) of 
	{ok, Fd} ->
	    write_to_file(Fd, FileName, ClientIp, TrafficCounters);
	Error ->
	    Error
    end.

%% @hidden
%% write header with client ip and proceed
write_to_file(Fd, FileName, ClientIp, TrafficCounters) ->
    case io:fwrite(Fd, "Client IP: ~s~n", [inet:ntoa(ClientIp)]) of 
	ok ->
	    write_counters(Fd, FileName, TrafficCounters, 0);
	{error, _Reason} = Error->
	    file:close(Fd),
	    file:delete(FileName),
	    Error
    end.
    
%% @hidden
%% Write each Target IP - Counter of Bytes oairs
write_counters(Fd, FileName, [{TargetIp, CountBytes}|TrafficCounters], SumBytes) ->
    StrIp = inet:ntoa(TargetIp),
    StrCount = integer_to_list(CountBytes),
    case io:fwrite(Fd, "IP: ~s - ~s Bytes ~n", [StrIp, StrCount]) of 
	ok ->
	    write_counters(Fd, FileName, TrafficCounters, SumBytes + CountBytes); 
	{error, _Reason} = Error ->
	    file:close(Fd),
	    file:delete(FileName),
	    Error
    end;

write_counters(Fd, FileName, [], SumBytes) ->
    case io:fwrite(Fd, 
		   "---------------------------~n"
		   "Summarize - ~p Bytes ~n", [SumBytes]) of 
	ok ->
	    file:close(Fd);
	{error, _Reason} = Error ->
	    file:close(Fd),
	    file:delete(FileName),
	    Error
    end.
