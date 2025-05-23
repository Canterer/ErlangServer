%% Description: TODO: Add description to auth component
-module(auth_component).
-export([
	handle_event/4,
	start_auth/4,
	auth_ok/6,
	qq_auth_ok/12,
	auth_failed/4
]).

-include("base_component_shared.hrl").



%% 事件: 开始认证
start_auth(GateNode, GateProc,ServerId,UserAuth)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid,{start_auth,ServerId,UserAuth}).

%% 事件: 认证成功
auth_ok(GateNode, GateProc,ServerId,PlayerId,AccountName,IsAdult)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid,{auth_ok,ServerId,PlayerId,AccountName,IsAdult}).

qq_auth_ok(GateNode, GateProc,ServerId,UserId,UserName,LgTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey)->
	GatePid = GateProc,    %% proc name is the remote pid
	?ZSS(),
	?base_gen_statem:cast(GatePid,{qq_auth_ok,ServerId,UserId,UserName,LgTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey}).

auth_failed(GateNode, GateProc,ServerId,Reason)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {auth_failed,ServerId,Reason}).



handle_event(cast,{start_auth,ServerId,UserAuth},connected,StateData) ->
% handle_connected_state(cast,{start_auth,ServerId,UserAuth}, StateData) ->
	?ZSS(),
	case lists:member(ServerId,base_env_ets:get(serverids,[])) of
		true->
			put(serverid,ServerId),
			base_auth_processor_server:auth(node(),self(),ServerId,UserAuth);
		_->
			base_logger_util:info_msg("Server is id ~p~n",[ServerId]),
			self()!{kick_client,"error serverid"}
	end,
	?ZSS(),
	{next_state, authing, StateData};
handle_event(cast,{auth_failed,ServerId,Reason},authing,StateData) ->
% handle_authing_state(cast,{auth_failed,ServerId,Reason}, StateData) ->
	FailedMsg = #user_auth_fail_s2c{reasonid=Reason},
	SendData = login_pb:encode_proto_msg(user_auth_fail_s2c,FailedMsg),
	base_tcp_client_statem:send_data(self(), SendData),
	{next_state, connected,StateData};
%% authing * auth_ok -> rolelisting
%% 认证成功后直接发送分线服务器列表
%%handle_authing_state({auth_ok,{visitor,PlayerId},AccountName,IsAdult}, StateData) ->
%%	?base_gen_statem:cast(node(),self(), {role_create_request}),
%%	put(playerid, {visitor,PlayerId}),
%%	put(account,AccountName),
%%	put(adult,IsAdult),
%%	{next_state,rolelisting,StateData};
handle_event(cast,{auth_ok,ServerId,PlayerId,AccountName,IsAdult},authing,StateData) ->
% handle_authing_state(cast,{auth_ok,ServerId,PlayerId,AccountName,IsAdult}, StateData) ->
	RoleList = base_gate_op:get_role_list(AccountName,get(serverid)),
	SendData = login_pb:encode_proto_msg(player_role_list_s2c,#player_role_list_s2c{roles=RoleList}),
	base_tcp_client_statem:send_data(self(), SendData),

	% %%auto_name
	% case RoleList of
	% 	[]->
	% 		case autoname_op:init_autoname_s2c() of
	% 			{Gname,Bname}->
	% 				put(autoname,{Gname,Bname}),
	% 				Message = login_pb:encode_proto_msg(init_random_rolename_s2c,#init_random_rolename_s2c{bn=Bname,gn=Gname}),
	% 				base_tcp_client_statem:send_data(self(), Message);
	% 			_->
	% 				nothing
	% 		end;
	% 	_->
	% 		nothing
	% end,
	apply_component(autoname_component, init_autoname, [RoleList]),
	put(playerid, PlayerId),
	put(account,AccountName),
	put(adult,IsAdult),
	{next_state,rolelisting,StateData};
handle_event(cast,{qq_auth_ok,ServerId,UserId,AccountName,LgTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey},authing,StateData) ->
% handle_authing_state(cast,{qq_auth_ok,ServerId,UserId,AccountName,LgTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey}, StateData) ->
	RoleList = base_gate_op:get_role_list(AccountName,get(serverid)),
	{NickName,Gender,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level} = Info,
	SendData = login_pb:encode_proto_msg(player_role_list_s2c,#player_role_list_s2c{roles=RoleList,
																		nickname=NickName,
																		is_yellow_vip=Is_yellow_vip,
																		is_yellow_year_vip=Is_yellow_year_vip,
																		yellow_vip_level=Yellow_vip_level}),
	base_tcp_client_statem:send_data(self(), SendData),
	apply_component(autoname_component, init_autoname, [RoleList]),
	% %%auto_name
	% case RoleList of
	% 	[]->
	% 		case autoname_op:init_autoname_s2c() of
	% 			{Gname,Bname}->
	% 				put(autoname,{Gname,Bname}),
	% 				Message = login_pb:encode_proto_msg(init_random_rolename_s2c,#init_random_rolename_s2c{bn=Bname,gn=Gname}),
	% 				base_tcp_client_statem:send_data(self(), Message);
	% 			_->
	% 				nothing
	% 		end;
	% 	_->
	% 		nothing
	% end,
	
	put(playerid, UserId),
	put(account,AccountName),
	put(adult,true),
	put(ipaddress, UserIp),
	put(pf, Pf),
	put(gender, Gender),
	put(install, LgTime),
	put(nickname, NickName),
	put(is_yellow_vip, Is_yellow_vip),
	put(is_yellow_year_vip, Is_yellow_year_vip),
	put(yellow_vip_level, Yellow_vip_level),
	put(openid, OpenId),
	put(openkey, OpenKey),
	put(pfkey, PfKey),
	{next_state,rolelisting,StateData};
handle_event(cast,{auth_failed,Reason},gaming,StateData) ->
% handle_gaming_state(cast,{auth_failed,Reason}, StateData) ->
	FailedMsg = #user_auth_fail_s2c{reasonid=Reason},
	SendData = login_pb:encode_proto_msg(user_auth_fail_s2c,FailedMsg),
	base_tcp_client_statem:send_data(self(), SendData),
	{next_state, connected,StateData};
handle_event(cast,{auth_ok,_PlayerId,AccountName,IsAdult},gaming,StateData) ->
% handle_gaming_state(cast,{auth_ok,_PlayerId,AccountName,IsAdult}, StateData) ->
	put(playerid, AccountName),
	put(account,AccountName),
	RolePid  = {get(roleproc),get(mapnode)},
	base_role_processor:finish_visitor(RolePid,AccountName),
	self()! {needchangename},
	{next_state,gaming,StateData};
handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.