%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%%-------------------------------------------------------------------

-compile([{parse_transform, lager_transform}]).

-define(INFO, lager:info).

-define(ERROR, lager:error).

-define(WARNING, lager:warning).

-type target_ip() :: inet:ip_address().

-type client_ip() :: inet:ip_address().

-type client_port() :: inet:port_number().

%% binary view of client IP with prefix
-type client_ip_bin() :: binary().

%% binary view of target IP with prefix
-type target_ip_bin() :: binary().
