%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky
%%% @copyright (C) 2014
%%% @doc
%%%      Application behaviour implementation
%%% @end
%%% Created : 20. Nov 2014 10:08 PM
%%%-------------------------------------------------------------------
-module(nasoc_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, get_config/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    ensure_started(riakpool),
    ok = application:load(nasoc),
    Host = get_config(riak_host),
    Port = get_config(riak_port),
    ok = riakpool:start_pool(Host, Port),
    ensure_started(nasoc).

start(_StartType, _StartArgs) ->
    nasoc_sup:start_link().

stop(_State) ->
    application:stop(nasoc),
    application:stop(riakpool),
    ok.

get_config(Config) ->
    {ok, Value} = application:get_env(nasoc, Config),
    Value.

ensure_started(App) ->
    case application:start(App) of 
	ok -> ok;
	{error, {already_started, App}} -> ok;
	{error, Reason} -> exit({cannot_start, App, Reason})
    end.  
