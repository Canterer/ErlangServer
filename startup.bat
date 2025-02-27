cd ./ebin
start cmd /k "title [timer] & erl.exe    -name timer@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
start cmd /k "title [db] & erl.exe   -mnesia dir '"../dbfile/"'   -name db@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
start cmd /k "title [line] & erl.exe    -name line@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
start cmd /k "title [map1] & erl.exe  +S 1  -name map1@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
start cmd /k "title [map2] & erl.exe  +S 1  -name map2@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
start cmd /k "title [gate1] & erl.exe  +S 1  -name gate1@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
start cmd /k "title [cross] & erl.exe    -name cross@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
start cmd /k "title [gm] & erl.exe    -name gm@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
