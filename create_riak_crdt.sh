#!/bin/sh

## R + W > N - strong consistency

riak-admin bucket-type create nasoc_crdt_maps '{"props":{"datatype":"map"}}'
riak-admin bucket-type create nasoc_crdt_sets '{"props":{"datatype":"set"}}'

riak-admin bucket-type activate nasoc_crdt_maps
riak-admin bucket-type activate nasoc_crdt_sets

riak-admin bucket-type update nasoc_crdt_maps '{"props":{"n_val":5}}'
riak-admin bucket-type update nasoc_crdt_sets '{"props":{"n_val":5}}'

riak-admin bucket-type update nasoc_crdt_maps '{"props":{"w":3}}'
riak-admin bucket-type update nasoc_crdt_maps '{"props":{"r":3}}'
riak-admin bucket-type update nasoc_crdt_maps '{"props":{"dw":1}}'

riak-admin bucket-type update nasoc_crdt_sets '{"props":{"w":3}}'
riak-admin bucket-type update nasoc_crdt_sets '{"props":{"r":3}}'
riak-admin bucket-type update nasoc_crdt_sets '{"props":{"dw":1}}'

