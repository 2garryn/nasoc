nasoc
=====

Simple SOCKS5 Proxy in erlang

What is done:
-------------
* simple authentification without password

* CONNECT command to remote server

* BIND command to open connections from external side of proxy (NOT TESTED). Do you know software which uses multiple connections over socks proxy?

* Riak as statistic storage added. Now you can't disable statistic collecting 

TODO:
------------
* Ipv6 is not supported now.

* UDP_ASSOCIATE is not supported.

* Fail cases to unit tests

* Additional auth. methods like user/password with LDAP and etc

* Add behaviour of traffic statistic

* Add possibility to set backet names for riak backends

* Add festure to disable statistic

Configuration:
------------
Checkout this: https://github.com/2garryn/nasoc/blob/master/nasoc.config

Proxy traffic statistic stored in Riak(CRDT):
------------

Before usage of proxy you should execute __create_riak_crdt.sh__. It creates need CRDT date types (maps and sets) in your riak installation.
Nasoc creates two backets due its work, named __nasoc_app_crdt_all_clients_bkt__ and __nasoc_app_crdt_client_to_target_map_bkt__.I strongly recommended not to use these buckets for other appliations.

To get all client ips, connected to nasoc, use following function:
```erlang
nasoc_traffic_counter:list_clients().
```
To get detialized statistic for one client, enter to erlang console following (here you will get statistic for client with ip 127.0.0.1):
```erlang
nasoc_traffic_counter:export({127,0,0,1}).
```

To save statistic to file enter:
```erlang
nasoc_traffic_counter:export_to_file({127,0,0,1}, "/tmp/127001_statistic.txt").
```
This command saves detalized statistic for client with ip 127.0.0.1 to file /tmp/127001_statistic.txt.
If something goes wrong, file will be cleaned up.

Implementation of proxy statistic based on Riak (manually siblings resolution):
--------------------
It is not used now. But you can look at it as example of conflict resolution in Riak.
