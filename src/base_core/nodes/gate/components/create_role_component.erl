%% Description: TODO: Add description to create_role_component
-module(create_role_component).
-export([
	handle_event/4,
	role_list_request/2,
	role_create_request/5,
	role_create_success/3,
	role_create_failed/3
]).

-include("base_component_shared.hrl").
%% 事件: 列举角色请求
role_list_request(GateNode,GateProc)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {role_list_request}).

%% 事件: 创建角色请求
role_create_request(GateNode, GateProc,RoleName,Gender,ClassType)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {role_create_request,RoleName,Gender,ClassType}).

%% 事件: 创建角色成功
role_create_success(GateNode, GateProc,RoleInfo)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {role_create_success,RoleInfo}).

%% 事件: 创建角色失败
role_create_failed(GateNode, GateProc,Reason)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {role_create_failed,Reason}).


handle_event(cast,{role_create_request},rolelisting,StateData)->
% handle_rolelisting_state(cast,{role_create_request}, StateData)->
	{visitor,PlayerId} = get(playerid),
	AccountName= get(account),
	LoginTime = get(install),
	Gender = get(gender),
	NickName = get(nickname),
	Is_yellow_vip = get(is_yellow_vip),
	Is_yellow_year_vip = get(is_yellow_year_vip),
	Yellow_vip_level = get(yellow_vip_level),
	Pf = get(pf),
	?base_logger_util:info_msg("error base_gate_op:create_role/11 undefined"),
	case base_gate_op:create_role(PlayerId,AccountName,NickName,Gender,base_gate_op:trans_addr_to_list(get(clientaddr)),get(serverid),
							 LoginTime,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level,Pf) of
		{ok,RoleId}->SendData = login_pb:encode_proto_msg(create_role_sucess_s2c,#create_role_sucess_s2c{role_id=RoleId}),
					 base_tcp_client_statem:send_data(self(), SendData),
					 put(neednotchange,true), %%创建玩家的时候不需要自动提示,从完成注册中弹出
					 {next_state,logining,StateData};
		{_,Reason}->SendData = login_pb:encode_proto_msg(create_role_failed_s2c,#create_role_failed_s2c{reasonid=Reason}),
					base_tcp_client_statem:send_data(self(), SendData),
					{next_state,rolelisting,StateData}
	end;
handle_event(cast,{role_create_request,RoleName,Gender,ClassType},rolelisting,StateData)->
% handle_rolelisting_state(cast,{role_create_request,RoleName,Gender,ClassType}, StateData)->
	PlayerId = get(playerid),
	AccountName= get(account),
	QQGender = get(gender),
	NickName = get(nickname),
	LoginTime = get(install),
	Is_yellow_vip = get(is_yellow_vip),
	Is_yellow_year_vip = get(is_yellow_year_vip),
	Yellow_vip_level = get(yellow_vip_level),
	Pf = get(pf),
	case base_gate_op:create_role(PlayerId,AccountName,NickName,QQGender,RoleName,Gender,ClassType,base_gate_op:trans_addr_to_list(get(clientaddr)),get(serverid),
							 LoginTime,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level,Pf) of
		{ok,RoleId}->
			% autoname_op:create_role(RoleName,RoleId),
			apply_component(autoname_component, create_role, [RoleName,RoleId]),
			case base_env_ets:get2(baidu_post,app_secret,"") of
				""->
					nothing;
				_->
					baidu_post:post_role_rpc(AccountName,RoleName)
			end,
			SendData = login_pb:encode_proto_msg(create_role_sucess_s2c,#create_role_sucess_s2c{role_id=RoleId}),
			base_tcp_client_statem:send_data(self(), SendData),
			{next_state,logining,StateData};
		{_,Reason}->SendData = login_pb:encode_proto_msg(create_role_failed_s2c,#create_role_failed_s2c{reasonid=Reason}),
					base_tcp_client_statem:send_data(self(), SendData),
					{next_state,rolelisting,StateData}
	end;
handle_event(cast,{role_create_success,RoleInfo},rolelisting,StateData)->
% handle_rolelisting_state(cast,{role_create_success,RoleInfo}, StateData)->
	RoleList = get(role_list) ++ RoleInfo,
	put(role_list, RoleList),
	SendData = login_pb:encode_proto_msg(player_role_list_s2c,#player_role_list_s2c{roles=RoleList}),
	base_tcp_client_statem:send_data(self(),SendData),
	{next_state,rolelisting,StateData};
handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.