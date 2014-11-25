%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @doc
%%%    SOCKS5 protocol binary aliases
%%% @end
%%% @copyright (C) 2014
%%%-------------------------------------------------------------------

%% SOCKS protocol version definition
-define(PROTO_VER5, 16#05).

%% Authentification methods
-define(NO_AUTH, 16#00).

-define(GSSAPI, 16#01).

%% authetifcation with user and password
-define(USER_PWD, 16#02).

%% Range of IANA auth. methods
-define(START_IANA, 16#03).

-define(END_IANA, 16#7F).

%% Range reserved for private auth. methods
-define(START_RSVD_FOR_PRIVATE, 16#80).

-define(END_RSVD_FOR_PRIVATE, 16#FE).

-define(NO_ACPT_METHODS, 16#FF).

%% Commands type
-define(CMD_CONNECT, 16#01).

-define(CMD_BIND, 16#02).

%% Commands reply
-define(CMD_NOT_SUPPORTED, 16#07).

-define(CMD_SUCCESS, 16#00).

-define(INTERNAL_ERROR, 16#01).

-define(NETWORK_UNREACH, 16#03).

-define(HOST_UNREACH, 16#04).

-define(CONN_REFUSED, 16#05).

-define(TTL_EXPIRED, 16#06).

-define(ANY_OTHER_ERROR, 16#FF).

%% AType definitions
-define(ATYPE_IPV6, 16#04).

-define(ATYPE_IPV4, 16#01).

-define(ATYPE_DOMAIN, 16#03).

-define(ATYPE_NOT_SUPPORTED,  16#08).

%% RSV (reserved)
-define(RSV, 16#00).

