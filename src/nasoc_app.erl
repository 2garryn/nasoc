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
    ok = application:start(nasoc).

start(_StartType, _StartArgs) ->
    nasoc_sup:start_link().

stop(_State) ->
    ok.

get_config(Config) ->
    {ok, Value} = application:get_env(nasoc, Config),
    Value.
