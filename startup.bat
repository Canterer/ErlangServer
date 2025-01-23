cd ./ebin
start cmd /k "title [timer] & erl.exe    -name timer@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
rem start cmd /k "title [db] & erl.exe   -mnesia dir '"../dbfile/"'   -name db@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
rem start cmd /k "title [line] & erl.exe    -name line@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
rem start cmd /k "title [map1] & erl.exe  -smp disable  -name map1@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
rem start cmd /k "title [map2] & erl.exe  -smp disable  -name map2@127.0.0.1 -s base_server_tool run --line line@127.0.0.1"
