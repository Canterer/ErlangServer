@echo off
chcp 65001
echo 谨慎使用我会全编译!!!!!!!!!
pause

rem cd %~dp0/..
set script_path=%~dp0
echo %script_path%
cd %0/../../../../..
echo %cd%

rem for /l %%i in (1,3,5) do echo %%i
rem dir /b /ad > folder_names.txt
rem dir /b /ad | findstr "base_core"

set SRC_PATH=src
cd %SRC_PATH%
echo 源文件目录: %SRC_PATH%

del ..\ebin\*.beam

rem python erlc.py --I ../include --src %SRC_PATH% --output ../ebin --nowarning true --define debug --debug true
E://SystemSofts//Python38//python.exe %script_path%erlc.py --src ../%SRC_PATH% --output ../ebin --nowarning true --define debug --debug true

pause