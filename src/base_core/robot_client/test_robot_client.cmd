cd /d %0/..
cd ../../../ebin

set IP=127.0.0.1
set ServerId=1
set ClientId=1

erl.exe -name robot_client_node@%IP% -eval "base_robot_client_tool:start_robot_client_test%s%('%IP%',%ServerId%,%ClientId%)"

pause
