%% Description: TODO: Add description to instance_op_component
-module(instance_op_component).
-export([
	handle_event/4,
	instance_into_world/3,
	change_line/1,
	change_map_in_same_node/7,
	change_map_in_other_node_end/4,
	transport/5,
	do_cleanup/2
]).

-include("base_component_shared.hrl").
-include("data_struct.hrl").
-include("game_map_define.hrl").
-include("little_garden.hrl").
-include("error_msg.hrl").
-include("role_struct.hrl").
-include("map_info_struct.hrl").
-include("instance_define.hrl").
-include("map_def.hrl").

% 角色进入地图
instance_into_world(MapId,LineId,RoleInfo)->
	%% 直接添加发送消息，改变位置消息
	Position =  get_pos_from_roleinfo(RoleInfo),
	IsInInstance = instance_op:is_in_instance(),
	?ZSS("IsInInstance:~p",[IsInInstance]),
	case IsInInstance of
		false->		
			case ?CHECK_INSTANCE_MAP(base_map_info_db:get_is_instance(base_map_info_db:get_map_info(MapId))) of
				true->			%%没在副本,但是在副本地图,数据存储有错误
%% 					{RespawnMapId,Pos} = mapop:get_respawn_pos(base_map_db_util:make_db_name(?DEFAULT_MAP)),
                    %%目前没有../maps，暂时将此时复活点放在{300,{175,175}}
                    {RespawnMapId,Pos} = 
							case mapop:get_respawn_pos(base_map_db_util:make_db_name(?DEFAULT_MAP)) of
								[]->{300,{175,175}};
								% MapPos->MapPos;
								_->{300,{175,175}}
								end,
					transport(RoleInfo, get(map_info),LineId,RespawnMapId,Pos);
				_->				%%未在副本地图
					case apply_component(server_travels_component,is_share_maps,[MapId],false) of
						true->				%%玩家在跨服地图,传送到跨服地图
							transport(RoleInfo, get(map_info),LineId,MapId,Position);
						_->
							?ZSS("Position:~p map_db:~p",[Position,get(map_db)]),
							case apply_component(mapop,check_pos_is_valid,[Position,get(map_db)],false) of
							% case mapop:check_pos_is_valid(Position,get(map_db)) of	
								false->						%%检查坐标是否在不可行走区域
									?ZSS(),
									% {RespawnMapId,Pos} =  mapop:get_respawn_pos(get(map_db)), 
									{RespawnMapId,Pos} = apply_component(mapop,get_respawn_pos,[get(map_db)],{MapId,Position}),
									transport(RoleInfo, get(map_info),LineId,RespawnMapId,Pos);
								true->
									?ZSS(),
									Message = role_packet:encode_role_map_change_s2c(Position, MapId,LineId),
									% send_data_to_gate(Message)
									apply_component(send_to_gate_component,send_data_to_gate,[Message])
							end
					end	
			end;
		true->
			% 内部触发 base_map_processor_server:join_instance 角色加入地图
			instance_op:on_line_by_instance(MapId,LineId,get(map_info))	
	end.

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.

update_role_info()->
	update_role_info(get(roleid),get(creature_info)).	
update_role_info(RoleId, RoleInfo) ->
	base_role_manager:regist_role_info(RoleId, RoleInfo).

transport(RoleInfo, MapInfo,LineId,MapId,{X,Y} = Coord) ->
		?ZSS("RoleInfo:~p, MapInfo:~p, LineId:~p, MapId:~p, Coord:~p",[RoleInfo,MapInfo,LineId,MapId,Coord]),
        OldMapNode = get_node_from_mapinfo(MapInfo),
        case get_best_map(MapId,LineId) of
			{ok,{NewLineId,MapNode,MapProcName}}->
				?ZSS("NewLineId:~p ,MapNode:~p ,MapProcName:~p ,OldMapNode:~p",[NewLineId,MapNode,MapProcName,OldMapNode]),
				case OldMapNode =:= MapNode of
					true ->
						change_map_in_same_node(MapInfo,MapNode,MapProcName,MapId,NewLineId,X,Y);
					false ->
						change_map_in_other_node_begin(MapInfo,MapNode,MapProcName,MapId,NewLineId,X,Y)
				end;
			{error}->
				Message = role_packet:encode_map_change_failed_s2c(?ERRNO_JOIN_MAP_ERROR_MAPID),
				% send_data_to_gate(Message),
				apply_component(send_to_gate_component,send_data_to_gate,[Message]),
				base_logger_util:info_msg("MAP CHANGE failed base_line_manager_server:get_map_name error!!!!!!!!!\n")
		end. 

get_best_map(MapId,LineIdOri)->
	case (LineIdOri =:= ?INSTANCE_LINEID) and instance_op:is_in_instance() of		%%transport in instance
		true->
			LineId = instance_op:get_old_line();
		_->
			LineId = LineIdOri
	end,
	case base_line_manager_server:get_map_name(LineId, MapId) of
			{ok,{MapNode,MapProcName}}->{ok,{LineId,MapNode,MapProcName}};
			{error}-> %%当前地图对应的线路错误
				case base_line_manager_server:get_line_status(MapId)  of	 %%获取线路状态
					error-> {error};
					LineInfos->
						{NewLineId,_RoleCount}=base_temp_util:get_min_count_of_lines(LineInfos), %%得到最小人数的线路
						case base_line_manager_server:get_map_name(NewLineId, MapId) of %%再次查询地图和节点
							{ok,{MapNode,MapProcName}}->{ok,{NewLineId,MapNode,MapProcName}};
							{error}-> {error}
						end
				end
	end.


on_change_map(_NewMapId,_LineId,NewMapProcName)->
	map_script:run_script(on_leave),
	instance_op:on_change_map(NewMapProcName),
	%%这个操作可能会导致玩家进程状态变化,由map_complate重新进入地图的时候,再设置玩家进程状态
	% role_sitdown_op:hook_on_action_sync_interrupt(base_timer_server:get_correct_now(),leave_map).	
	apply_component(role_sitdown_op,hook_on_action_sync_interrupt,[base_timer_server:get_correct_now(),leave_map]).	

leave_map(RoleInfo,MapInfo)->
	set_move_timer(0),
	% buffer_op:stop_mprecover(),
	apply_component(buffer_op,stop_mprecover,[]),
	% buffer_op:stop_hprecover(),
	apply_component(buffer_op,stop_hprecover,[]),
	creature_op:leave_map(RoleInfo, MapInfo),
	object_update:send_pending_update().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 在同一节点更换地图
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_map_in_same_node(MapInfo,NewNode,NewMapProcName,NewMapId,LineId,X,Y) ->
	?ZSS(),
	on_change_map(NewMapId,LineId,NewMapProcName),
	RoleInfo = get(creature_info),
	leave_map(RoleInfo, MapInfo),
	GS_system_map_info = #gs_system_map_info{map_id=NewMapId,
						 line_id=LineId, 
						 map_proc=NewMapProcName, 
						 map_node=NewNode},                                    
	put(map_info, create_mapinfo(NewMapId, LineId,NewNode, NewMapProcName, ?GRID_WIDTH)),	
	put(creature_info, set_path_to_roleinfo(get(creature_info), [])),
	put(creature_info, set_pos_to_roleinfo(get(creature_info), {X, Y})),
	put(creature_info, set_mapinfo_to_roleinfo(get(creature_info), GS_system_map_info)), 
	update_role_info(get(roleid), get(creature_info)),
	%%node未改变,需要更新Line,MapId
	role_pos_util:update_role_line_map(get(roleid),LineId,NewMapId),
	% NpcInfoDB = npc_op:make_npcinfo_db_name(NewMapProcName),
	NpcInfoDB = apply_component(npc_op,make_npcinfo_db_name,[NewMapProcName]),
	put(npcinfo_db,NpcInfoDB),
	Map_db = base_map_db_util:make_db_name(NewMapId),
	put(map_db,Map_db),			
	put(last_req_pos,{X, Y}),
	%%通知guild节点 更新line mapid
	% guild_op:change_map(LineId,NewMapId),
	apply_component(guild_op,change_map,[LineId,NewMapId]),
	notify_gate_map_change(RoleInfo, NewMapId),
	notify_client_map_change(NewMapId, LineId, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 在不同一节点更换地图: 开始阶段
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_map_in_other_node_begin(MapInfo, NewNode, NewMapProcName, NewMapId, LineId, X, Y) ->
	?ZSS(),
	on_change_map(NewMapId,LineId,NewMapProcName),
	RoleInfo = get(creature_info),
	Gs_New_Map_info = set_proc_to_mapinfo(MapInfo, NewMapProcName),
	Gs_New_Map_info1 = set_node_to_mapinfo(Gs_New_Map_info, NewNode),
	Gs_New_Map_info2 = set_mapid_to_mapinfo(Gs_New_Map_info1, NewMapId),
	Gs_New_Map_info3 = set_lineid_to_mapinfo(Gs_New_Map_info2,LineId),
	apply_component(role_server_travel,hook_on_trans_map_by_node,[Gs_New_Map_info3]),
	RoleOpCopy = apply_component(role_op_copy_component,export_for_copy,[]),
	case base_role_manager:start_copy_role(Gs_New_Map_info3, RoleInfo, get(gate_info),X,Y,RoleOpCopy) of
		error->
			apply_component(role_server_travel,hook_on_trans_map_faild,[]),
			Message = role_packet:encode_map_change_failed_s2c(?ERRNO_JOIN_MAP_ERROR_MAPID),
			% send_data_to_gate(Message),	
			apply_component(send_to_gate_component,send_data_to_gate,[Message]),					
			base_logger_util:info_msg("change_map_in_other_node_begin error ~p ~n",[get(roleid)]);
		ok->
			%%先把内存拷贝过去,才离开当前地图,所以在do_cleanup(change_map)里不能做改变内存的操作
			do_cleanup(change_map,get(roleid)),
			base_role_manager:stop_self_process(get(map_info),node(),get(roleid))
	end.
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 在不同一节点更换地图: 切换阶段
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_map_in_other_node_end(RoleInfo, MapInfo, X, Y) ->
        %%向http_client 发送更改map消息
	MapInfo = get(map_info),
	LineId = get_lineid_from_mapinfo(MapInfo),
	NewMapId = get_mapid_from_mapinfo(MapInfo),
	%%通知guild节点 更新line mapid
	guild_op:change_map(LineId,NewMapId),
	%% 通知服务器通知客户端
	notify_gate_map_change(RoleInfo, NewMapId),
	notify_client_map_change(NewMapId, LineId, X, Y).

notify_client_map_change(NewMapId, LineId, X, Y) ->
	% send_data_to_gate(role_packet:encode_role_map_change_s2c({X,Y}, NewMapId, LineId)).
	Message = role_packet:encode_role_map_change_s2c({X,Y}, NewMapId, LineId),
	apply_component(send_to_gate_component,send_data_to_gate,[Message]).

notify_gate_map_change(RoleInfo, NewMapId) ->
	GateProc = get_proc_from_gs_system_gateinfo(get(gate_info)),
	GateNode = get_node_from_gs_system_gateinfo(get(gate_info)),
	RoleId = creature_op:get_id_from_creature_info(RoleInfo),
	base_tcp_client_statem:mapid_change(GateNode, GateProc, node(), NewMapId, make_role_proc_name(RoleId)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 进程退出做一些清理工作:Tag:change_map / uninit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_cleanup(Tag,RoleId)->
		MapInfo = get(map_info),
		RoleInfo = get(creature_info),						
		try	
			case Tag of 
				uninit->				%%下线不会执行change_map里的script on_leave,所以这里要主动掉一下.	
					map_script:run_script(on_leave);
				_->
					nothing
			end,	
			leave_map(RoleInfo, MapInfo)		%%离开地图
		catch
			E1:R1-> base_logger_util:info_msg("base_role_op:do_cleanup creature_op:leave_map error ~p:~p~n",[E1,R1])
		end,			
		try
			% uninit(Tag,RoleId)							%%卸载
			apply_component(role_uninit_component,uninit,[Tag,RoleId])
		catch
			E2:R2-> base_logger_util:info_msg("base_role_op:do_cleanup uninit error ~p:~p ~p ~n",[E2,R2,erlang:get_stacktrace()])
		end,	
		if
			Tag =:= uninit-> 
				%%从全局角色信息中卸载
				role_server_travel:hook_on_offline(),
				role_pos_db:unreg_role_pos_to_mnesia(RoleId);
			true->
				nothing
		end,
		try
			%%从ets卸载
			base_role_manager:unregist_role_info(RoleId),
			pet_manager:unregist_pet_info(pet_op:get_out_pet_id())
		catch
			E4:R4-> base_logger_util:info_msg("base_role_op:do_cleanup unregist_role_info error ~p:~p~n",[E4,R4])
		end.


make_role_proc_name(RoleId)->
	list_to_atom(integer_to_list(RoleId)).

set_move_timer(NewTimer)->
	case get(move_timer)of
		undefined->
			put(move_timer,NewTimer);
		0->
			put(move_timer,NewTimer);
		Timer->
			erlang:cancel_timer(Timer),
			put(move_timer,NewTimer)
	end.

change_line(LineId)->
	MapInfo = get(map_info),
	RoleInfo = get(creature_info),
	MapId = get_mapid_from_mapinfo(MapInfo ),
	OldMapNode = get_node_from_mapinfo(MapInfo),
	{X,Y} = get_pos_from_roleinfo(RoleInfo ),
	BaseCheck = (not instance_op:is_in_instance()) and (not is_dead()), 
	if
		BaseCheck->									%%副本中不允许换线
			case is_leave_attack() of
				true->
				    case base_line_manager_server:get_map_name(LineId, MapId) of
						{ok,{MapNode,MapProcName}}->
							case OldMapNode =:= MapNode of
								true ->				
									change_map_in_same_node(MapInfo,MapNode,MapProcName,MapId,LineId,X,Y);
								false ->				
									change_map_in_other_node_begin(MapInfo,MapNode,MapProcName,MapId,LineId,X,Y)
							end;
						_->
							nothing
					end;
				_->
					Message = role_packet:encode_map_change_failed_s2c(?ERROR_NOT_LEAVE_ATTACK),
					% send_data_to_gate(Message)
					apply_component(send_to_gate_component,send_data_to_gate,[Message])		
			end;
		true->
			nothing
	end.

is_dead()->
	creature_op:is_creature_dead(get(creature_info)).

is_leave_attack()->
	timer:now_diff(base_timer_server:get_correct_now(),get(leave_attack_time)) > ?LEAVE_ATTACK_TIME*1000.
