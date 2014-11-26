nasoc
=====

Simple SOCKS5 Proxy in erlang

What is done:
-------------
* simple authentification without password

* CONNECT command to remote server

* BIND command to open connections from external side of proxy (NOT TESTED). Do you know software which uses multiple connections over socks proxy?

TODO:
------------
* Ipv6 is not supported now.

* UDP_ASSOCIATE is not supported.

* Fail cases to unit tests

* Additional auth. methods like user/password with LDAP and etc

Configuration:
------------
Checkout this: https://github.com/2garryn/nasoc/blob/master/src/nasoc.app.src
