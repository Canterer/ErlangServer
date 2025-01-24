Map节点 
	等待line节点的base_line_manager_server启动服务
	启动服务base_map_manager_server
base_map_manager_server
	init初始化时，send_check_message出发handle_info的 global_line_check
	通过base_line_manager_server:regist_map_manager 注册当前启动的Map节点
		其中会触发base_line_manager_server:load_map，其为配置的lines列表中的每个lineId开启对于的map_processor
		lineId 一一对应 map节点
		base_map_manager_server:start_map_processor进而间接开启base_map_processor_server服务

base_map_processor_server
	开启后，会通过base_line_manager_server:regist_mapprocessor注册记录




数据相关细节
base_application_server每个节点都会强制开启服务，初始化会触发 base_db_tools:wait_ets_create()
备注：
	base_db_tools:wait_ets_create()会通过ets_operater_behaviour触发相关mod:create()方法
	根据当前Node()获取proc_allowed_nodes中定义的  procs ，代表当前节点上上述proc服务运行
	根据proc_ets_mods获取每个进程依赖的数据模块(继承ets_operater_behaviour),并触发数据模块的create方法

每个节点开启之后统一处理
	base_timer_server:start_at_app()//记录相对于timer节点的时间偏差
	applicationex:wait_ets_init()//请求服务，初始化当前节点的ets配置列表，并
	base_global_util:wait_global_proc_register(),


base_db_tools:wait_for_tables_loop  可区分本地、db节点
	通过mnesia:wait_for_tables循环等待数据表就绪

base_db_tools:wait_ets_create会创建ets
base_db_tools:wait_ets_init会先等待db_tables(mnesia数据库表)就绪后再初始化

base_db_tools:wait_for_all_db_tables() 
	通过wait_tables_in_dbnode()先循环等待db节点的表就绪
	通过wait_for_local_ram_tables()循环等待本地节点的表就绪

	根据nodes_ram_table获取每个节点是否依赖ram数据模块
	通过db_operater_behaviour收集需要的数据表，数据从名为DB_MOD_TABLE的ets中取



编译流程
	每个xxx_app.erl指定生成 ebin/路径下的  yyy.app



base_global_proc_util:wait_global_proc_register 开启base_global_proc_checker_server服务
	根据global_wait_proc获取各个节点的 全局进程
	根据当前节点，获取本节点的全局进程，并通过base_global_proc_checker_server:is_all_node_waite_finish注册记录在GLOBAL_PROC_ETS
	这里主要时记录proc与节点间映射关系，方便后续直接通过proc名字远程调用对应节点的进程



db节点
	只存在三个服务base_db_master_server、base_db_line_master_server、base_db_dmp_server
	其中master对应disc_tables数据、line对应ram_tables数据，dmp对应统一修改Mnesia数据库接口
	三个服务开启的节点分别为db节点、line节点、map节点
	
	base_db_master_server通过base_db_init_util:db_init_master()初始化
	base_db_line_master_server通过base_db_init_util:db_init_line_master()初始化

	两者都依赖db_operater_behaviour:start()收集数据表
	base_db_ini_util:db_init_master()
	base_db_ini_util:db_init_line_master()
	base_db_tools:wait_line_db()
	以上上个接口会触发db_operater_behaviour的start方法，触发相关的mod:start()，并将自身记录进DB_MOD_TABLE中

	base_db_dmp_server服务主要循环延迟触发base_db_dmp_util:flush_not_using 每五分钟
	base_db_dmp_util主要是两个表每5分钟循环使用，并定时将其中一个表通过flush_data_rpc函数移除数据并异步写入db节点的数据库中
	base_db_dmp_util主要是记录数据操作，只写不读

	base_db_dal_util提供读写数据接口，read读本地数据库、read_rpc读db节点的数据库
	
	

	dp节点只依赖timer节点，开启base_db_master_server服务后，
	通过base_db_tools:wait_for_tables_loop 以及mnesia:system_info(tables)等待表就绪


line节点
	line节点是中心节点，供其他节点连接。开启两个服务：manager-processor
	manager开启根据lines配置的列表，开启processor
	每个processor初始化时，通过base_lines_manager:regist_to_manager()注册自己，将自身记录在ETS_LINE_PROC_DB中
	
	
map节点
	开启两个服务：manager_processor
	第一步：
	通过base_lines_manager:regist_map_manager注册自己，将自身记录在ETS_MAP_MANAGER_DB中
	并load_map(Node);//Node指map类节点，例如map1节点、map2节点
	每个map节点开启后，根据id搭配LineID开启processor。
	通过base_line_manager_server:start_map_processor开启processor。
	通过base_map_info_db:get_maps_bylinetag(LineId)获取MapIDs，针对每个MapID开启processor。
	通过base_map_manager_server:start_map_processor(MapNode，LineID, MapId, map)开启map_processor，并
	通过base_line_manager_server:regist_map_processor({node(), LineId, MapId，MapName})最终通过line_processor:do_regist()注册
	将相关数据记录在MAP_PROC_DB中。

	第二步：
	遍历所有的MapInfo，为初次base_map_db:load_map_file(MapDataId，MapDb)
	其会将maps/map_xxxx文件中的内容加载进MapId_db表中

	每个map节点与一个line_processor相关联，可开启多个map_processor。通过LineId可获取到其关联的多个MapId
	配置中lines_info 记录了每一个line上对应的map节点名以及其关联的多个MapId。	
	
	每个base_map_processor_server初始化后，通过map_db_pprocessor:make_db_name(MapId)。
	
	map节点负责开启role_app

base_map_info_db
	初始化时通过db_operater_behaviour:init_ets(map_info, ?MAP_INFO_ETS, #map_info.mapid)通过数据库读取至表
base_map_db
	base_map_processor_server初始化时使用base_map_db:load_map_file从文件中读取内容写入ets中
	
	
xxx_db.erl 用于管理数据
xxx_op.erl 用于协议的数据操作接口
xxx_packet.erl	用于erlang数据封装成包packet，实质依赖login_pb.erl

