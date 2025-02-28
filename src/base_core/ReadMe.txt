application:get_env(Opt)
	application_controller:get_pid_env(group_leader(), Opt).

文件目录说明：

--base_include 用于include 基本的定义文件hrl

--base_module 用于包装基本的opt行为模块

--behaviour_mods 用于自定义行为 主要是 db_operater_behaviour、ets_operater_behaviour

--modules 相关的工具模块  xxx_util 或 base_db_tools、base_env_ets、login_pb 
	包含各种基本的工具模块 如 base_logger_util(打印日志)、base_node_util(节点相关接口)
	--db  针对db相关的辅助接口
	--mnesia 用于命令行读取config文件中的 配置表文件(以txt保持) 构建mnesia数据库对应的table
	--protocol 定义协议相关的配置信息(包含各种协议)
	--socket 主要用关于 分发各种协议的package数据包

--nodes 包含各种底层逻辑的节点

--option 节点相关的配置文件，主要用于初始化env_ets环境配置，便于读取数据

--scripts 存放不同逻辑的脚本文件
	--make 用于全量编译erl文件
	--mnesia 用于读取本地配置表文件 生成本地mnesia数据库的数据文件
	--test_gm 用于启动gm_client
	--version 用于版本更新脚本

--server 底层框架相关的服务器模块
	--application 提供统一启动app的服务
	--global_proc 用于提供全局模块的进程
	--role 角色相关
	--server_control 用于控制运行中的服务器

--robot_client 用于模拟client连接 测试服务器
