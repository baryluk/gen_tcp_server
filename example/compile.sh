#!/bin/bash
erlc ../gen_tcp_server*.erl
cp ../gen_tcp_server*.beam ./
erlc -pa . example*.erl
