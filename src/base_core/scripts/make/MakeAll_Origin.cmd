@echo off

echo 谨慎使用我会全编译!!!!!!!!!
pause

del ..\ebin\*.beam

cd /d %0/..


set PROTO_PATH=..\..\..\common\proto
rem call ../../../tools/ei_compiler/erlang.cmd

rem cd /d %0/..
 
cd ..\scripts

rem python makeversion.py -DEBUG

python erlc.py --I ../include --src ../src/ --output ../ebin --nowarning true --define debug --debug true

pause

