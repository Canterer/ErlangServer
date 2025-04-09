%% Description: TODO: Add description to request_line_info_component
-module(request_line_map_component).
-export([
	handle_event/4,
	role_into_map_request/4,
	role_into_map_success/1,
	line_info_request/3,
	line_info_success/3
]).

-include("base_component_shared.hrl").
-include("data_struct.hrl").
-include("game_map_define.hrl").

%% 事件: 创建地图请求
role_into_map_request(GateNode, GateProc, RoleId,LineId)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {role_into_map_request,RoleId,LineId}).

%% 事件: 准备进入地图
role_into_map_success(GatePid) ->
	?base_gen_statem:cast(GatePid, {role_into_map_success}).

%% 事件: 获取分线服务器信息请求
line_info_request(GateNode,GateProc,MapId)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid,{line_info_request,MapId}).

%% 事件: 获取分线服务器信息成功
line_info_success(GateNode,GateProc,LineInfos)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {line_info_success,LineInfos}).

handle_event(cast,{line_info_request,MapId},rolelisting,StateData)->
% handle_rolelisting_state(cast,{line_info_request,MapId},StateData)->
	async_get_line_info_by_mapid(MapId),
	{next_state,rolelisting,StateData};
handle_event(cast,{line_info_success,LineInfos},rolelisting,StateData)->
% handle_rolelisting_state(cast,{line_info_success,LineInfos},StateData)->
	LineInfoByRecord = linesinfo_to_record(LineInfos),
	SendData = login_pb:encode_proto_msg(role_line_query_ok_s2c,#role_line_query_ok_s2c{lines=LineInfoByRecord}),
	base_tcp_client_statem:send_data(self(),SendData),
	{next_state,logining,StateData};
handle_event(cast,{role_into_map_request,RoleId,_LineId},logining,StateData) ->
% handle_logining_state(cast,{role_into_map_request,RoleId,_LineId},StateData) ->
	RoleList = base_gate_op:get_role_list(get(account),get(serverid)),
	?ZSS("role_into_map_request RoleList:~p",[RoleList]),
	case lists:member(RoleId,[base_pb_util:get_role_id_from_logininfo(RoleInfo) || RoleInfo <- RoleList]) of
		true->
			case role_pos_util:where_is_role(RoleId) of
				[]->
					?ZSS(),
					%%由于line是客户端发来的,有可能会与当前地图线路不符.所以重新再请求一次线路,自动选择最优线路
					Mapid = base_gate_op:get_last_mapid(RoleId),
					put(roleid, RoleId),
					put(mapid, Mapid),
					async_get_line_info_by_mapid(Mapid);
				RolePos ->
					?ZSS(),
					base_logger_util:info_msg("Role_id:[~p], is exist~n", [RoleId]),
					RoleNode = role_pos_db:get_role_mapnode(RolePos),
					RoleProc = role_pos_db:get_role_pid(RolePos),
					case base_role_manager:stop_role_processor(RoleNode,RoleId, RoleProc,other_login) of
						{error,{noproc,_}}->				%%进程已经不在了,直接删除该玩家残留  zhangting
							role_pos_db:unreg_role_pos_to_mnesia(RoleId);
						_->
							nothing
					end,	
					Message = role_packet:encode_other_login_s2c(),
					base_tcp_client_statem:send_data_after(self(),Message,1000)
			end;
		_-> 
			base_logger_util:info_msg("hack find !!! error roleid!!! Account ~p RoleId ~p ~n",[get(account),RoleId]),
			self()!{kick_client}
	end,
	{next_state,logining,StateData};
handle_event(cast,{line_info_success,LineInfos},logining,StateData)->
% handle_logining_state(cast,{line_info_success,LineInfos}, StateData)->
	{LineId,_OnlineRole}=base_temp_util:get_min_count_of_lines(LineInfos),
	put(lineid, LineId),
%% 	start_game_after_line_fixed(LineId),
	start_game_after_line_fixed(1),				%%枫少修改只有一线
	{next_state,logining,StateData};
handle_event(cast,{role_into_map_success},logining,StateData) ->
% handle_logining_state(cast,{role_into_map_success}, StateData) ->
	{next_state, gaming,StateData};
handle_event(cast,{line_info_request,MapId},gaming,StateData)->
% handle_gaming_state(cast,{line_info_request,MapId}, StateData)->
	async_get_line_info_by_mapid(MapId),
	{next_state,gaming,StateData};
handle_event(cast,{line_info_success,LineInfos},gaming,StateData)->
% handle_gaming_state(cast,{line_info_success,LineInfos},StateData)->
	LineInfoByRecord = linesinfo_to_record(LineInfos),
	SendData = login_pb:encode_proto_msg(role_line_query_ok_s2c,#role_line_query_ok_s2c{lines=LineInfoByRecord}),
	base_tcp_client_statem:send_data(self(),SendData),
	{next_state,gaming,StateData};
handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.




linesinfo_to_record(LineInfos)->
	lists:map(fun(X)->{LineId,RoleCount}=X, 
			  #li{lineid=LineId,rolecount=RoleCount}
		  end, LineInfos).

% 根据MapId 100 查询line状态
async_get_line_info_by_mapid(MapId)->
	?ZSS(),
	case base_map_info_db:get_map_info(MapId) of
		[]->
			?ZSS(),
			base_line_manager_server:query_line_status(node(),self() ,MapId);
		MapInfo->
			?ZSS(),
			case ?CHECK_INSTANCE_MAP(base_map_info_db:get_is_instance(MapInfo)) of
				true->
					%%发给客户端当前线路,如果是副本地图,只提供线1供登录,登录之后再决定再转到副本地图所在
					LineInfos = [{1,0}],
					?ZSS(),
					line_info_success(node(),self(),LineInfos);
				_->
					?ZSS(),
					base_line_manager_server:query_line_status(node(),self() ,MapId)
			end
	end.

start_game_after_line_fixed(LineId)->
	?ZSS(),
	MapId = get(mapid),
	RoleId = get(roleid),
	AccountName= get(account),
	LoginTime = get(install),
	LoginIp = base_gate_op:trans_addr_to_list(get(clientaddr)),
	Gender = get(gender),
	NickName = get(nickname),
	% Is_yellow_vip = get(is_yellow_vip),
	Is_yellow_year_vip = get(is_yellow_year_vip),
	Yellow_vip_level = get(yellow_vip_level),
	Pf = get(pf),
	
	base_gate_op:update_account_info(AccountName, LoginTime, LoginIp, NickName, Gender, Pf, Is_yellow_year_vip, Yellow_vip_level),
	GateProc = self(),%%get(procname),
	case base_line_manager_server:get_map_name(LineId, MapId) of
		{ok,{MapNodeTmp,MapProcNameTmp}}->
			case server_travels_util:is_share_map_node(MapNodeTmp) of
				true->			%%玩家在跨服地图上,由于到从本地数据库中加载玩家数据,所以先在本地节点启动,之后再转移过去
					[MapNode|_] = base_line_manager_server:get_map_nodes(),
					MapProcName = undefined;
				_->
					MapNode = MapNodeTmp,
					MapProcName = MapProcNameTmp
			end; 
		_->
			%%玩家在副本中,先在本地节点启动,之后再转移过去
			MapProcName = undefined,
			[MapNode|_] = base_line_manager_server:get_map_nodes()
	end,
	GS_system_map_info = #gs_system_map_info{map_id=MapId,
								 line_id=LineId, 
								 map_proc=MapProcName,		%%这里的map_proc有可能是undefined 
								 map_node=MapNode},
	GS_system_role_info = #gs_system_role_info{role_id = RoleId},
	GS_system_gate_info = #gs_system_gate_info{gate_proc = GateProc, gate_node=node(), gate_pid=self()},
	New_GS_system_role_info = GS_system_role_info#gs_system_role_info{role_node=MapNode},
	put(gs_system_role_info, New_GS_system_role_info),
	base_logger_util:info_msg("base_role_manager:start_one_role ~p ~p ~p ~n",[GS_system_gate_info,New_GS_system_role_info,GS_system_map_info]),
	base_role_manager:start_one_role(GS_system_map_info, New_GS_system_role_info, GS_system_gate_info,
								{get(account),get(pf),get(adult),LoginIp,get(is_yellow_vip), get(is_yellow_year_vip),
								 get(yellow_vip_level),get(openid),get(openkey),get(pfkey)}).