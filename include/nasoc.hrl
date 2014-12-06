%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2014
%%%-------------------------------------------------------------------

%% TODO: add logger to file 
-define(INFO, error_logger:info_msg).

-define(ERROR, error_logger:error_msg).

-define(WARNING, error_logger:warning_msg).

-type target_ip() :: inet:ip_address().

-type client_ip() :: inet:ip_address().

-type client_port() :: inet:port_number().

%% result of term_to_binary(client_ip())
-type client_ip_bin() :: binary().

%% result of term_to_binary(target_ip())
-type target_ip_bin() :: binary().
