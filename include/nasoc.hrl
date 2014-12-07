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

%% result of term_to_binary(client_ip())
-type client_ip_bin() :: binary().

%% result of term_to_binary(target_ip())
-type target_ip_bin() :: binary().
