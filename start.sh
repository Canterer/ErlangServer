#!/bin/bash
cd ./ebin
ulimit -SHn 65535 && erl +P 100000 +K true   -detached -name timer@127.0.0.1 -s server_tool run --line line@127.0.0.1

