server_update.py 用于远程连接  服务端主机  并进行更新。主要是上传文件，调用start.sh stop_all.py

貌似有两套 开启/终止 脚本命令

start.py stop_all.py   主要用于server_update.py远程使用
	其中由于脚本是在服务端主机上，所以脚本里的ip为127.0.0.1本地主机地址

run.py stop.py 由autocfg.cmd动态生成的脚本
	通过调用autocfg.cmd传递的参数，来配置脚本里的ip地址。