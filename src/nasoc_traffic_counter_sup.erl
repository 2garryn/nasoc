%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%     Traffic counters supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(nasoc_traffic_counter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{nasoc_traffic_counter, {nasoc_traffic_counter, start_link, []},
            temporary, infinity, worker, [nasoc_traffic_counter]}]}}.
