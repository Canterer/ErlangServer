@echo off
chcp 65001
echo 读取Config文件夹中的静态配置表(txt文件),创建并初始化本地db节点的Mnesia表
cd %0/../../../../../ebin
echo %cd%
pause

mkdir ..\dbfile
del /q ..\dbfile\* 

erl -name db@127.0.0.1 -noshell -s base_mnesia_gen_tables_util run -s init stop
xcopy Mnesia.db@127.0.0.1 ..\dbfile\
rd /s /q Mnesia.db@127.0.0.1

pause