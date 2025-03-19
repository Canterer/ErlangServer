-module(base_tcp_client_fsm).

% -behaviour(gen_fsm).

%% External exports
-export([start_link/2,
	 send_data/2,
	 shutown_client/1]).

% -export([init/1, handle_event/3,handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([connecting/3,socket_ready/3,socket_disable/3]).


-export([connected/3,
	 start_auth/4]).

-export([authing/3,
	 auth_ok/6,auth_failed/4]).

-export([rolelisting/3,
	 role_list_request/2,
	 role_create_request/5,role_create_success/3,
	 role_create_failed/3]).

-export([logining/3,
	 line_info_success/3]).

-export([preparing_into_map/3,
	 env_prepared/3]).

-export([gaming/3,
	 line_info_request/3]).

-export([kick_client/1]).

-compile(export_all).

-define(OBJECT_PACKET_UPDATE_INTERVAL,500).	%%500ms
-define(TIME_OUT_MICROS,300000000).		%%300s超时
-define(TIME_OUT_CHECK_INTERVER,100000).	%%100s
-record(state, {}).
-include("base_gen_statem_shared.hrl").
-include("network_setting.hrl").
-include("login_def.hrl").
-include("login_pb.hrl").
-include("data_struct.hrl").
-include("game_map_define.hrl").

%% ====================================================================
%% External functions
%% ====================================================================
start_link(OnReceiveData,OnClientClose)->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	base_gen_statem:start_link(?MODULE, [OnReceiveData,OnClientClose], []).

do_init([OnReceiveData,OnClientClose]) ->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	base_timer_server:start_at_process(),
	process_flag(trap_exit, true),
	put(on_receive_data, OnReceiveData),
	put(on_close_socket, OnClientClose),
	packet_object_update:init(),
	{A,B,C} = os:timestamp(),
	base_logger_util:msg("~p:~p os:timestamp=~p~n",[?MODULE,?FUNCTION_NAME,os:timestamp()]),
	rand:seed(exsplus,{A,B,C}),
	erlang:send_after(?TIME_OUT_CHECK_INTERVER,self(),{alive_check}),
	erlang:send_after(?OBJECT_PACKET_UPDATE_INTERVAL,self(),{object_update_interval}),
	{ok, connecting, #state{}}.

object_update_create(GatePid,CreateData)->	
	base_rpc_util:cast(GatePid,{object_update_create,CreateData}).
object_update_delete(GatePid,DelData)->	
	base_rpc_util:cast(GatePid,{object_update_delete,DelData}).
object_update_update(GatePid,UpdateData)->	
	base_rpc_util:cast(GatePid,{object_update_update,UpdateData}).		
send_pending_object_update(GatePid)->	
	base_rpc_util:cast(GatePid,{send_pending_update}).

send_data_after(GatePid,Data,TimeMs)->
	try 
		erlang:send_after(TimeMs, GatePid, {send_to_client,Data})
	catch 
		_E:_R->
		base_logger_util:msg("~p~n", [erlang:get_stacktrace()])
	end.	

send_data(GatePid,Data) ->
	base_rpc_util:cast(GatePid, {send_to_client,Data}).
		
send_data_filter(GatePid,CurData,FltData)->
	base_rpc_util:cast(GatePid,{send_to_client_filter,CurData,FltData}).
	
shutown_client(Pid) ->
	Pid!{shutdown}.

kick_client(GatePid)->
	base_rpc_util:cast(GatePid, {kick_client}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 事件触发函数
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% 事件: 列举角色请求
role_list_request(GateNode,GateProc)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {role_list_request}).
	
%%autoname reset
reset_random_rolename(_GateNode,GateProc)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {reset_random_rolename}).

%% 事件: 创建角色请求
role_create_request(GateNode, GateProc,RoleName,Gender,ClassType)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {role_create_request,RoleName,Gender,ClassType}).

%% 事件: 创建角色成功
role_create_success(GateNode, GateProc,RoleInfo)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {role_create_success,RoleInfo}).

%% 事件: 创建角色失败
role_create_failed(GateNode, GateProc,Reason)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {role_create_failed,Reason}).

%% 事件: 创建地图请求
role_into_map_request(GateNode, GateProc, RoleId,LineId)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {role_into_map_request,RoleId,LineId}).

%% 事件: 获取分线服务器信息成功
line_info_success(GateNode,GateProc,LineInfos)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {line_info_success,LineInfos}).

%% 事件: 获取分线服务器信息请求
line_info_request(GateNode,GateProc,MapId)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid,{line_info_request,MapId}).

%% 事件: 角色进程启动
role_process_started(GatePid, MapNode,RoleProc)->
	try
		% gen_fsm:sync_send_all_state_event(GatePid, {role_process_started,MapNode,RoleProc})
		base_gen_statem:call(GatePid, {role_process_started,MapNode,RoleProc})
	catch
		_:_->
			error
	end.		
		

%%  事件: socket已经连接
socket_ready(GateNode,GateProc,ClientSocket)->
	?ZS_LOG(),
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {socket_ready,ClientSocket}).

socket_disable(GateNode,GateProc,ClientSocket)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {socket_disable,ClientSocket}).

%% 事件: 开始认证
start_auth(GateNode, GateProc,ServerId,UserAuth)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid,{start_auth,ServerId,UserAuth}).

%%start_auth(GateNode, GateProc,Time,AuthResult)->
%%	GatePid = GateProc,    %% proc name is the remote pid
%%	base_gen_statem:cast(GatePid,{start_auth,Time,AuthResult}).

%% 事件: 认证成功
auth_ok(GateNode, GateProc,ServerId,PlayerId,AccountName,IsAdult)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid,{auth_ok,ServerId,PlayerId,AccountName,IsAdult}).

qq_auth_ok(GateNode, GateProc,ServerId,UserId,UserName,LgTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid,{qq_auth_ok,ServerId,UserId,UserName,LgTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey}).

auth_failed(GateNode, GateProc,ServerId,Reason)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid, {auth_failed,ServerId,Reason}).

%% 事件: 认证失败
env_prepared(GateNode,GateProc,Info)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_gen_statem:cast(GatePid,{env_prepared,Info}).

%% 事件: 准备进入地图
role_into_map_success(GatePid) ->
	base_gen_statem:cast(GatePid, {role_into_map_success}).

mapid_change(GateNode, GateProc, MapNode,MapId,RoleProc)->
	GatePid = GateProc,    %% proc name is the remote pid
	base_rpc_util:cast(GatePid,{mapid_change,MapNode,MapId,RoleProc}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connecting(EventType,EventContent,StateData)->
	base_logger_util:msg("~p:~p(EventType=~p,EventContent=~p,StateData=~p)~n",[?MODULE,?FUNCTION_NAME,EventType,EventContent,StateData]),
	handle_connecting_state(EventType,EventContent,StateData).

connected(EventType,EventContent,StateData)->
	base_logger_util:msg("~p:~p(EventType=~p,EventContent=~p,StateData=~p)~n",[?MODULE,?FUNCTION_NAME,EventType,EventContent,StateData]),
	handle_connected_state(EventType,EventContent,StateData).

authing(EventType,EventContent,StateData)->
	base_logger_util:msg("~p:~p(EventType=~p,EventContent=~p,StateData=~p)~n",[?MODULE,?FUNCTION_NAME,EventType,EventContent,StateData]),
	handle_authing_state(EventType,EventContent,StateData).

rolelisting(EventType,EventContent,StateData)->
	base_logger_util:msg("~p:~p(EventType=~p,EventContent=~p,StateData=~p)~n",[?MODULE,?FUNCTION_NAME,EventType,EventContent,StateData]),
	handle_rolelisting_state(EventType,EventContent,StateData).

logining(EventType,EventContent,StateData)->
	base_logger_util:msg("~p:~p(EventType=~p,EventContent=~p,StateData=~p)~n",[?MODULE,?FUNCTION_NAME,EventType,EventContent,StateData]),
	handle_logining_state(EventType,EventContent,StateData).

preparing_into_map(EventType,EventContent,StateData)->
	base_logger_util:msg("~p:~p(EventType=~p,EventContent=~p,StateData=~p)~n",[?MODULE,?FUNCTION_NAME,EventType,EventContent,StateData]),
	handle_preparing_into_map_state(EventType,EventContent,StateData).

gaming(EventType,EventContent,StateData)->
	base_logger_util:msg("~p:~p(EventType=~p,EventContent=~p,StateData=~p)~n",[?MODULE,?FUNCTION_NAME,EventType,EventContent,StateData]),
	handle_gaming_state(EventType,EventContent,StateData).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 状态：连接中
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_connecting_state(cast,{socket_ready,CliSocket},StateData)->
	?ZS_LOG(),
	case base_gate_op:check_socket(CliSocket) of
		false->
			?ZS_LOG(),
			self()!{kick_client,"black ip"},
			put(clientsock, CliSocket),
			{next_state, connected, StateData};
		true->
			?ZS_LOG(),
			FPacketProcRe = 
				case gen_tcp:recv(CliSocket,4,180000) of			%%timeout 3 min
					{ok,?CROSS_DOMAIN_FLAG_HEADER}->
						?ZS_LOG(),
						base_logger_util:msg("client port policy-file-request failed,game proc ~n"),
						case gen_tcp:recv(CliSocket,erlang:byte_size(?CROSS_DOMAIN_FLAG)-4,10000) of
							{ok,_}->
								?ZS_LOG(),
								gen_tcp:send(CliSocket,base_crossdomain_server:make_normal_cross_file()),
								true;
							{error,closed}->
								?ZS_LOG(),
								stop;
							_->
								?ZS_LOG(),
								false	
						end;
					{ok, <<"tgw_">>} ->	
						?ZS_LOG(),
						filter_tgw(CliSocket),
						send_data(self(), login_pb:encode_tgw_gateway_s2c(#tgw_gateway_s2c{})),
						true; 
					{ok,RecvBin}->
						?ZS_LOG(),
						<< PacketLenth:?PACKAGE_HEADER_BIT_LENGTH/big,LeftHeaderBin/binary >> = RecvBin,
						base_logger_util:msg("PacketLenth ~p RecvBin ~p ~n",[PacketLenth,RecvBin]),
						
						case gen_tcp:recv(CliSocket,PacketLenth - 2,10000) of
							{ok,LeftBin}->
								FirstPackageBin = <<LeftHeaderBin/binary,LeftBin/binary>>,
								self() ! {tcp, CliSocket, FirstPackageBin},
								?ZS_LOG(),
								true;	
							{error,closed}->
								?ZS_LOG(),
								stop;
							Errno2->
								{ok, {PeerIPAddressTmp, _}} = inet:peername(CliSocket),
								base_logger_util:msg("socket_ready recv ERROR ~p ip ~p !!!! ~n",[Errno2,PeerIPAddressTmp]),
								false	
						end;                       
					{error,closed}->
						?ZS_LOG(),
						stop;
					Errno->
						base_logger_util:msg("socket_ready recv ERROR ~p !!!! ~n",[Errno]),
						false
						
				end,
				%base_logger_util:msg("yanzengyan, FPacketProcRe: ~p ~n",[FPacketProcRe]),
				case FPacketProcRe of
					true->		
						inet:setopts(CliSocket, ?TCP_CLIENT_SOCKET_OPTIONS),
						{ok, {PeerIPAddress, _Port}} = inet:peername(CliSocket),
						?ZS_LOG(),
						put(clientsock, CliSocket),
						put(clientaddr, PeerIPAddress),
						{next_state, connected, StateData};
					stop->
						?ZS_LOG(),
						{stop, normal, StateData};
					false->
						base_logger_util:msg("first packet proc failed,kick client ~n"),
						put(clientsock, CliSocket),
						self()!{kick_client,"first packet error"},
						{next_state, connected, StateData}
				end							
	end;
handle_connecting_state(cast,{socket_disable,CliSocket},StateData)->
	self()!{kick_client,"socket is disable "},
	put(clientsock, CliSocket),
	{stop, normal, StateData};
handle_connecting_state(_EventType,Event,StateData)->
	do_handle_state_event(_EventType,Event,connecting,StateData).
	% {next_state, connecting, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 状态：已连接
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_connected_state(cast,{start_auth,ServerId,UserAuth}, StateData) ->
	case lists:member(ServerId,base_env_ets:get(serverids,[])) of
		true->
			put(serverid,ServerId),
			base_auth_processor_server:auth(node(),self(),ServerId,UserAuth);
		_->
			base_logger_util:msg("Server is id ~p~n",[ServerId]),
			self()!{kick_client,"error serverid"}
	end,	
	{next_state, authing, StateData};
%%handle_connected_state({start_auth,Time,AuthResult}, StateData) ->
%%	base_auth_processor_server:auth(node(),self(),Time,AuthResult),
%%	{next_state, authing, StateData};
handle_connected_state(EventType,Event,StateData) ->
	do_handle_state_event(EventType,Event,connected,StateData).
	% {next_state, connected, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  状态: 认证中
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_authing_state(cast,{auth_failed,ServerId,Reason}, StateData) ->
	FailedMsg = #user_auth_fail_s2c{reasonid=Reason},
	SendData = login_pb:encode_user_auth_fail_s2c(FailedMsg),
	send_data(self(), SendData),
	{next_state, connected,StateData};
%% authing * auth_ok -> rolelisting
%% 认证成功后直接发送分线服务器列表
%%handle_authing_state({auth_ok,{visitor,PlayerId},AccountName,IsAdult}, StateData) ->
%%	base_gen_statem:cast(node(),self(), {role_create_request}),
%%	put(playerid, {visitor,PlayerId}),
%%	put(account,AccountName),
%%	put(adult,IsAdult),
%%	{next_state,rolelisting,StateData};
handle_authing_state(cast,{auth_ok,ServerId,PlayerId,AccountName,IsAdult}, StateData) ->
	RoleList = base_gate_op:get_role_list(AccountName,get(serverid)),
	SendData = login_pb:encode_player_role_list_s2c(#player_role_list_s2c{roles=RoleList}),
	send_data(self(), SendData),
	%%auto_name
	case RoleList of
		[]->
			case autoname_op:init_autoname_s2c() of
				{Gname,Bname}->
					put(autoname,{Gname,Bname}),
					Message = login_pb:encode_init_random_rolename_s2c(#init_random_rolename_s2c{bn=Bname,gn=Gname}),
					send_data(self(), Message);
				_->
					nothing
			end;
		_->
			nothing
	end,
	put(playerid, PlayerId),
	put(account,AccountName),
	put(adult,IsAdult),
	{next_state,rolelisting,StateData};
handle_authing_state(cast,{qq_auth_ok,ServerId,UserId,AccountName,LgTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey}, StateData) ->
	RoleList = base_gate_op:get_role_list(AccountName,get(serverid)),
	{NickName,Gender,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level} = Info,
	SendData = login_pb:encode_player_role_list_s2c(#player_role_list_s2c{roles=RoleList,
																		  nickname=NickName,
																		  is_yellow_vip=Is_yellow_vip,
																		  is_yellow_year_vip=Is_yellow_year_vip,
																		  yellow_vip_level=Yellow_vip_level}),
	send_data(self(), SendData),
	%%auto_name
	case RoleList of
		[]->
			case autoname_op:init_autoname_s2c() of
				{Gname,Bname}->
					put(autoname,{Gname,Bname}),
					Message = login_pb:encode_init_random_rolename_s2c(#init_random_rolename_s2c{bn=Bname,gn=Gname}),
					send_data(self(), Message);
				_->
					nothing
			end;
		_->
			nothing
	end,
	
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
handle_authing_state(EventType,Event,StateData) ->
	do_handle_state_event(EventType,Event,authing,StateData).
	% {next_state, authing, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 列举角色状态
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_rolelisting_state(cast,{role_create_request}, StateData)->
	{visitor,PlayerId} = get(playerid),
	AccountName= get(account),
	LoginTime = get(install),
	Gender = get(gender),
	NickName = get(nickname),
	Is_yellow_vip = get(is_yellow_vip),
	Is_yellow_year_vip = get(is_yellow_year_vip),
	Yellow_vip_level = get(yellow_vip_level),
	Pf = get(pf),
	case base_gate_op:create_role(PlayerId,AccountName,NickName,Gender,base_gate_op:trans_addr_to_list(get(clientaddr)),get(serverid),
							 LoginTime,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level,Pf) of
		{ok,RoleId}->SendData = login_pb:encode_create_role_sucess_s2c(#create_role_sucess_s2c{role_id=RoleId}),
					 send_data(self(), SendData),
					 put(neednotchange,true), %%创建玩家的时候不需要自动提示,从完成注册中弹出
					 {next_state,logining,StateData};
		{_,Reason}->SendData = login_pb:encode_create_role_failed_s2c(#create_role_failed_s2c{reasonid=Reason}),
					send_data(self(), SendData),
					{next_state,rolelisting,StateData}
	end;
handle_rolelisting_state(cast,{role_create_request,RoleName,Gender,ClassType}, StateData)->
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
			autoname_op:create_role(RoleName,RoleId),
			case base_env_ets:get2(baidu_post,app_secret,"") of
				""->
					nothing;
				_->
					baidu_post:post_role_rpc(AccountName,RoleName)
			end,
			SendData = login_pb:encode_create_role_sucess_s2c(#create_role_sucess_s2c{role_id=RoleId}),
			send_data(self(), SendData),
			{next_state,logining,StateData};
		{_,Reason}->SendData = login_pb:encode_create_role_failed_s2c(#create_role_failed_s2c{reasonid=Reason}),
					send_data(self(), SendData),
					{next_state,rolelisting,StateData}
	end;
handle_rolelisting_state(cast,{role_create_success,RoleInfo}, StateData)->
	RoleList = get(role_list) ++ RoleInfo,
	put(role_list, RoleList),
	SendData = login_pb:encode_player_role_list_s2c(#player_role_list_s2c{roles=RoleList}),
	send_data(self(),SendData),
	{next_state,rolelisting,StateData};
handle_rolelisting_state(cast,{line_info_request,MapId},StateData)->
	async_get_line_info_by_mapid(MapId),
	{next_state,rolelisting,StateData};
handle_rolelisting_state(cast,{line_info_success,LineInfos},StateData)->
	LineInfoByRecord = linesinfo_to_record(LineInfos),
	SendData = login_pb:encode_role_line_query_ok_s2c(#role_line_query_ok_s2c{lines=LineInfoByRecord}),
	send_data(self(),SendData),
	{next_state,logining,StateData};
handle_rolelisting_state(cast,{reset_random_rolename}, StateData)->
	%%auto_name
	put(autoname,[]),
	case autoname_op:init_autoname_s2c() of
		{Gname,Bname}->
			Message = login_pb:encode_init_random_rolename_s2c(#init_random_rolename_s2c{bn=Bname,gn=Gname}),
			send_data(self(), Message);
		_->
			nothing
	end,
	{next_state, rolelisting, StateData};
handle_rolelisting_state(EventType,Event,StateData) ->
	do_handle_state_event(EventType,Event,rolelisting,StateData).
	% {next_state, rolelisting, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  事件: 获取分线服务器信息成功
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
handle_logining_state(cast,{role_into_map_request,RoleId,_LineId},StateData) ->
	RoleList = base_gate_op:get_role_list(get(account),get(serverid)),
	case lists:member(RoleId,[ pb_util:get_role_id_from_logininfo(RoleInfo) || RoleInfo <- RoleList]) of
		true->
			case role_pos_util:where_is_role(RoleId) of
				[]->
					%%由于line是客户端发来的,有可能会与当前地图线路不符.所以重新再请求一次线路,自动选择最优线路
					Mapid = base_gate_op:get_last_mapid(RoleId),
					put(roleid, RoleId),
					put(mapid, Mapid),
					async_get_line_info_by_mapid(Mapid);
				RolePos ->
					base_logger_util:msg("Role_id:[~p], is exist~n", [RoleId]),
					RoleNode = role_pos_db:get_role_mapnode(RolePos),
					RoleProc = role_pos_db:get_role_pid(RolePos),
					case base_role_manager:stop_role_processor(RoleNode,RoleId, RoleProc,other_login) of
						{error,{noproc,_}}->				%%进程已经不在了,直接删除该玩家残留  zhangting
							role_pos_db:unreg_role_pos_to_mnesia(RoleId);
						_->
							nothing
					end,	
					Message = role_packet:encode_other_login_s2c(),
					send_data_after(self(),Message,1000)
			end;
		_-> 
			base_logger_util:msg("hack find !!! error roleid!!! Account ~p RoleId ~p ~n",[get(account),RoleId]),
			self()!{kick_client}
	end,
	{next_state,logining,StateData};
handle_logining_state(cast,{line_info_success,LineInfos}, StateData)->
	{LineId,_OnlineRole}=line_util:get_min_count_of_lines(LineInfos),
	put(lineid, LineId),
%% 	start_game_after_line_fixed(LineId),
	start_game_after_line_fixed(1),				%%枫少修改只有一线
	{next_state,logining,StateData};
handle_logining_state(cast,{role_into_map_success}, StateData) ->
	{next_state, gaming,StateData};
handle_logining_state(EventType,Event,StateData) ->
	do_handle_state_event(EventType,Event,logining,StateData).
	% {next_state, logining, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 状态：进入地图
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% env_prepared X preparing_into_map -> gaming 
handle_preparing_into_map_state(cast,{env_prepared,Info},StateData)->
	{next_state,gaming,StateData};
handle_preparing_into_map_state(EventType,Event, StateData) ->
	do_handle_state_event(EventType,Event,preparing_into_map,StateData).
	% {next_state, preparing_into_map, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  状态：玩游戏
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%
handle_gaming_state(cast,{line_info_request,MapId}, StateData)->
	async_get_line_info_by_mapid(MapId),
	{next_state,gaming,StateData};
handle_gaming_state(cast,{line_info_success,LineInfos},StateData)->
	LineInfoByRecord = linesinfo_to_record(LineInfos),
	SendData = login_pb:encode_role_line_query_ok_s2c(#role_line_query_ok_s2c{lines=LineInfoByRecord}),
	send_data(self(),SendData),
	{next_state,gaming,StateData};
handle_gaming_state(cast,{auth_failed,Reason}, StateData) ->
	FailedMsg = #user_auth_fail_s2c{reasonid=Reason},
	SendData = login_pb:encode_user_auth_fail_s2c(FailedMsg),
	send_data(self(), SendData),
	{next_state, connected,StateData};
handle_gaming_state(cast,{auth_ok,_PlayerId,AccountName,IsAdult}, StateData) ->
	put(playerid, AccountName),
	put(account,AccountName),
	RolePid  = {get(roleproc),get(mapnode)},
	base_role_processor:finish_visitor(RolePid,AccountName),
	self()! {needchangename},
	{next_state,gaming,StateData};
handle_gaming_state(EventType,Event,StateData)->
	do_handle_state_event(EventType,Event,gaming,StateData).
	% {next_state,gaming,StateData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_callback_mode()->
	state_functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 异步事件处理: send by gen_fsm:sync_send_all_state_event/2,3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_handle_event(cast, stop, _StateName, _StateData)->
% handle_event(stop, StatName , StateData)->
	% {stop, normal, StateData};
	{stop, normal};
do_handle_event(cast, _EventContent, _StateName, _StateData)->
% handle_event(Event, StateName, StateData) ->
	% {next_state, StateName, StateData};
	{keep_state_and_data,[]};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 同步事件处理: send by gen_fsm:send_all_state_event/2,3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_handle_event({call, From}, {role_process_started,MapNode,RoleProc}, StateName, StateData) ->
% handle_sync_event({role_process_started,MapNode,RoleProc}, From, StateName, StateData) ->
	put(mapnode, MapNode),
	put(roleproc, RoleProc),
	%% add 
	{ChatNode,ChatProc} = start_chat_role(),
	put(chatnode,ChatNode),
	put(chatproc,ChatProc),
	case get(neednotchange) of
		undefined->
			NeedChangeName = base_role_db:name_can_change(base_role_db:get_role_info(get(roleid))),
				%%可以改名
			case NeedChangeName of
				true->  self()! {needchangename};
				_-> noting
			end;
		_-> nothing
	end,
	% {reply, {ChatNode,ChatProc}, StateName, StateData};
	{keep_state_and_data, [{reply, From, {ChatNode,ChatProc}}]};
do_handle_event({call, From}, _EventContent, StateName, _StateData) ->
% handle_sync_event(Event, From, StateName, StateData) ->
	Reply = ok,
% 	{reply, Reply, StateName, StateData}.
	{keep_state_and_data, [{reply, From, Reply}]};

do_handle_event(info, {mapid_change,MapNode,MapId,RoleProc}, StateName, StateData) ->
% handle_info({mapid_change,MapNode,MapId,RoleProc}, StateName, StateData) ->
	put(mapnode, MapNode),
	put(mapid, MapId),
	put(roleproc, RoleProc),
	% {next_state,StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {alive_check}, StateName, StateData) ->
% handle_info({alive_check}, StateName, StateData) ->
	case get(alive_time) of
		undefined->
			put(alive_time,os:timestamp()),
			erlang:send_after(?TIME_OUT_CHECK_INTERVER,self(),{alive_check});
		Timer->
			case timer:now_diff(os:timestamp(),Timer) > ?TIME_OUT_MICROS of			%%超时
				true->
					self() ! {tcp_closed, 0};
				_->
					erlang:send_after(?TIME_OUT_CHECK_INTERVER,self(),{alive_check}),
					nothing
			end
	end,
	% {next_state, StateName, StateData};
	keep_state_and_data;
%%
%% game gate need to send data
do_handle_event(info, {send_to_client, Data}, StateName, StateData) ->
% handle_info({send_to_client, Data}, StateName, StateData) ->
	%%TODO
	try erlang:binary_to_term(Data) of
		Val-> base_logger_util:msg("send_to_role ~p~n",[erlang:binary_to_term(Data)])
	catch
		_:_->
			<<ID:16, Data1/binary>> = Data
%% 			base_logger_util:msg("send_to_role@@ [~p]~n",[ID])
	end,
	erlang:port_command(get(clientsock), Data, [force]),
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {send_to_client_filter, Cur_Binary,Flt_Binary}, StateName, StateData) ->
% handle_info({send_to_client_filter, Cur_Binary,Flt_Binary}, StateName, StateData) ->
	IpAddr = get(clientaddr),
	case whiteip:match(IpAddr) of
		true->
			IsFilter = false;
		_-> 
			case blackip:match(IpAddr) of
				true-> 
					IsFilter = true;
				_->
					IsFilter = false
			end
	end,
	
	case IsFilter of
		true->
			%%@@ base_logger_util:msg("send_to_role ~p~n",[erlang:binary_to_term(Flt_Binary)]),			
			erlang:port_command(get(clientsock), Flt_Binary, [force]);
		_->
			%%@@ base_logger_util:msg("send_to_role ~p~n",[erlang:binary_to_term(Cur_Binary)]),			
			erlang:port_command(get(clientsock), Cur_Binary, [force])
	end,
	
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {do_recv}, StateName, StateData) ->
% handle_info({do_recv}, StateName, StateData) ->	
	%%RolePid = rpc:call(get(mapnode), erlang, whereis, [get(roleproc)]),
	RolePid  = {get(roleproc),get(mapnode)},
	send_recv_message_queue(RolePid),
	%% å¤è¯»æ¯å¦è¿è¦send_after
	case get(recv_message_queue) of
		undefined -> 
			put(recv_message_queue, []);
		[] -> 
			nothing;
		_Any ->
			RecvTimerRef = erlang:send_after(40,self(),{do_recv}),
			put(recv_timer_ref, RecvTimerRef)	
	end,
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {tcp, Socket, BinData}, StateName, StateData) ->
% handle_info({tcp, Socket, BinData}, StateName, StateData) ->
	put(alive_time,os:timestamp()),
	{M,F,A} = get(on_receive_data),
	%%RolePid = rpc:call(get(mapnode), erlang, whereis, [get(roleproc)]),
	RolePid  = {get(roleproc),get(mapnode)},
	try	
		send_recv_message_queue(RolePid),		
		apply(M,F,A++[self(), BinData, RolePid])
	catch		
		Type:Reason ->				
				base_logger_util:msg("tcp recv exception: ~p BinData~p ~n",[{Type,Reason},BinData]),
				put_recv_Message(BinData)		
	end,
		
	inet:setopts(Socket, [{active, once}]),
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {tcp_closed, _Socket}, StateName, StateData) ->
% handle_info({tcp_closed, _Socket}, StateName, StateData) ->
	do_clear_on_close(),
	% {stop, normal, StateData};
	stop;
do_handle_event(info, {shutdown}, StateName, StateData) ->
% handle_info({shutdown},StateName,StateData)->
	do_clear_on_close(),
	% {stop, normal, StateData};
	stop;
%%no player logout
do_handle_event(info, {kick_client}, StateName, StateData) ->
% handle_info({kick_client},StateName,StateData)->
	base_logger_util:msg("receive need kick client, maybe error client!\n"),
	close_and_clear_no_logout(),
	% {stop,normal, StateData};
	stop;
do_handle_event(info, {kick_client,KickInfo}, StateName, StateData) ->
% handle_info({kick_client,KickInfo},StateName,StateData)->
	base_logger_util:msg("receive need kick client, Reason:~p!\n",[KickInfo]),
	do_clear_on_close(),
	% {stop,normal, StateData};
	stop;
do_handle_event(info, {needchangename}, StateName, StateData) ->
% handle_info({needchangename},StateName,StateData)->
	 SendData = login_pb:encode_visitor_rename_s2c(#visitor_rename_s2c{}),
	 send_data(self(), SendData),
	% {next_state,StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {object_update_create,CreateData}, StateName, StateData) ->
% handle_info({object_update_create,CreateData}, StateName, StateData) ->
	packet_object_update:push_to_create_data(CreateData),
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {object_update_delete,DelData}, StateName, StateData) ->
% handle_info({object_update_delete,DelData}, StateName, StateData) ->
	packet_object_update:push_to_delete_data(DelData),
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {object_update_update,UpdateData}, StateName, StateData) ->
% handle_info({object_update_update,UpdateData}, StateName, StateData) ->
	packet_object_update:push_to_update_data(UpdateData),
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {object_update_interval}, StateName, StateData) ->
% handle_info({object_update_interval}, StateName, StateData) ->
	packet_object_update:send_pending_update(),
	erlang:send_after(?OBJECT_PACKET_UPDATE_INTERVAL,self(),{object_update_interval}),
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, {send_pending_update}, StateName, StateData) ->
% handle_info({send_pending_update}, StateName, StateData) ->
	packet_object_update:send_pending_update(),
	% {next_state, StateName, StateData};
	keep_state_and_data;
do_handle_event(info, _EventContent, _StateName, _StateData) ->
% handle_info(_Info, StateName, StateData) ->
	% {next_state, StateName, StateData}.
	keep_state_and_data.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
do_terminate(Reason, StateName, StatData) ->
	%%add for terminat
	{M,F,A} = get(on_close_socket),
	apply(M,F,A++[get(clientsock), playercontex]),
	do_clear_on_close(),
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
do_code_change(OldVsn, StateName, StateData, Extra) ->
	{ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
send_left_msg()->
	case get(message_queue) of
		undefined -> 
			put(message_queue, []);
		[] -> 
			nothing;
		Any ->
			Messages = erlang:list_to_binary(Any),
			base_logger_util:msg("send_to_role ~p~n",[Any]),				
			erlang:port_command(get(clientsock), Messages, [force]),
			put(message_queue, [])
	end.

clear_game_player()->
	case base_rpc_util:is_process_alive(get(mapnode), get(roleproc)) of
		false ->
			nothing;
		true ->
			base_role_manager:stop_role_processor(get(mapnode), get(roleid), get(roleproc),uninit)
	end.
	
close_port()->
	try
		erlang:port_close(get(clientsock))
	catch
		_E:_R->
			nothing
	end.	

close_and_clear_no_logout()->
	send_left_msg(),
	close_port(),
	stop_chat_role().
	
do_clear_on_close()->
	send_left_msg(),
	close_port(),
	clear_game_player(),
	stop_chat_role().

inton({A,B,C,D},P)->
	integer_to_list(A) ++ "_" ++ integer_to_list(B) ++ "_" ++ 
		integer_to_list(C) ++ "_" ++ integer_to_list(D) 
		++ "_" ++ integer_to_list(P).

linesinfo_to_record(LineInfos)->
	lists:map(fun(X)->{LineId,RoleCount}=X, 
			  #li{lineid=LineId,rolecount=RoleCount}
		  end, LineInfos).

%% 启动聊天进程
start_chat_role()->
   base_logger_util:msg("start_chat_role~n"),
   case base_line_manager_server:get_chat_name() of
		[]->
			base_logger_util:msg("start_chat_role get chatnode name err!!!!!!~n"),
			{node(),self()};
		{ChatNode,_Proc,_Count}->	
			RoleId = get(roleid),
			GateProc = self(),%%get(procname),
			MapNode = get(mapnode),
			RoleProc = get(roleproc),
			GS_system_role_info = #gs_system_role_info{role_id = RoleId, role_pid = RoleProc, role_node =MapNode},
			GS_system_gate_info = #gs_system_gate_info{gate_proc = GateProc, gate_node=node(), gate_pid=self()},			
			case chat_manager:start_chat_role(ChatNode, GS_system_role_info, GS_system_gate_info) of
				error->
					base_logger_util:msg("chat_manager:start_chat_role error ~n"),
					self() ! {tcp_closed, 0},
					{0,0};
				Re->
					Re	
			end
	end.					  

stop_chat_role()->
	ChatProc = get(chatproc),
	RoleId = get(roleid),
	case get(chatnode) of
		undefined->
			nothing;
		ChatNode->  
			chat_manager:stop_chat_role(ChatNode, ChatProc,RoleId)
	end.  


%%å¤çroleprocessorå´©æºçæåµ
put_recv_Message(CurrentM) ->
	case get(recv_message_queue) of
		undefined ->
			PreviousMessageQueue = [],
			put(recv_message_queue, []);
		_Any ->
			PreviousMessageQueue = _Any
	end,	
	
	MessageQ = lists:reverse([CurrentM] ++ PreviousMessageQueue),
	put(recv_message_queue, MessageQ),
	
	%% è®¾ç½®åéæ¶é´	
	case get(recv_timer_ref) of
		undefined ->
			RecvTimerRef = erlang:send_after(40,self(),{do_recv}),
			put(recv_timer_ref, RecvTimerRef);
		RecvRef ->
			case erlang:read_timer(RecvRef) of
				false ->
					RecvTimerRef = erlang:send_after(40,self(),{do_recv}),
					put(recv_timer_ref, RecvTimerRef);
				_Time ->
					{ok}
			end			
	end.	

send_recv_message_queue(Pid)	->
	{M,F,A} = get(on_receive_data),
	case get(recv_message_queue) of
		undefined -> 
			put(recv_message_queue, []);
		[] -> 
			nothing;
		NewMsgQueue ->				
			S = fun(DataBin)->
				try													
					apply(M,F,A++[self(), DataBin, Pid])
				catch
						Type:Reason ->
							base_logger_util:msg("RoleId: ~p tcp do_recv error: ~p ~p~n",[get(roleid),{Type,Reason},erlang:get_stacktrace()])				
				end,
				NewMsgQueue = lists:delete(DataBin,get(recv_message_queue)),
				put(recv_message_queue,NewMsgQueue)						
			end,				
			lists:foreach(S, NewMsgQueue)
	end.			
	
async_get_line_info_by_mapid(MapId)->
	case base_map_info_db:get_map_info(MapId) of
		[]->
			base_line_manager_server:query_line_status(node(),self() ,MapId);
		MapInfo->
			case ?CHECK_INSTANCE_MAP(base_map_info_db:get_is_instance(MapInfo)) of
				true->
					%%发给客户端当前线路,如果是副本地图,只提供线1供登录,登录之后再决定再转到副本地图所在
					LineInfos = [{1,0}],
					line_info_success(node(),self(),LineInfos);
				_->
					base_line_manager_server:query_line_status(node(),self() ,MapId)
			end
	end.
	
start_game_after_line_fixed(LineId)->
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
	%base_logger_util:msg("base_role_manager:start_one_role ~p ~p ~p ~n",[GS_system_gate_info,New_GS_system_role_info,GS_system_map_info]),
	base_role_manager:start_one_role(GS_system_map_info, New_GS_system_role_info, GS_system_gate_info,
								{get(account),get(pf),get(adult),LoginIp,get(is_yellow_vip), get(is_yellow_year_vip),
								 get(yellow_vip_level),get(openid),get(openkey),get(pfkey)}).

filter_tgw(CliSocket) ->
	Ret1 = gen_tcp:recv(CliSocket, 1, 10000),
	case Ret1 of
		{ok, <<"\r">>} ->
			Ret2 = gen_tcp:recv(CliSocket, 1, 10000),
			case Ret2 of
				{ok, <<"\n">>} ->
					Ret3 = gen_tcp:recv(CliSocket, 1, 10000),
					case Ret3 of
						{ok, <<"\r">>} ->
							Ret4 = gen_tcp:recv(CliSocket, 1, 10000),
								case Ret4 of
									{ok, <<"\n">>} ->
										ok;
									{ok, _} ->
										filter_tgw(CliSocket);
									Error ->
										failed
								end;
						{ok, _} ->
							filter_tgw(CliSocket);
						Error ->
							failed
					end;
				{ok, _} ->
					filter_tgw(CliSocket);
				Error ->
					failed
			end;
		{ok, _} ->
			filter_tgw(CliSocket);
		Error ->
			failed
	end.
