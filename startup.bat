cd ./ebin
start cmd /k "title [timer] & erl.exe    -name timer@127.0.0.1 -s server_tool run --line line@127.0.0.1"
