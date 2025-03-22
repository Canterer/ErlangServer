-module(base_role_manager).
-include("base_define_min.hrl").
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
 -define(ROLE_MANAGER,local_role_manager).
%% --------------------------------------------------------------------
%% External exports
-export([
	start_link/1,
	start_one_role/4,
	stop_role_processor/4
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile(export_all).

-include("data_struct.hrl").
-include("role_struct.hrl").
-include("map_info_struct.hrl").
-include("common_define.hrl").
-include("error_msg.hrl").

-record(state, {roles_db}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 自定义数据结构
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ====================================================================
%% External functions
%% ====================================================================
start_link(RoleDB)->
	?base_gen_server:start_link({local,?ROLE_MANAGER},?MODULE,[RoleDB],[]).

init([RoleDB]) ->
	base_timer_server:start_at_process(),
	?base_ets:new(role_profile, [set, public, named_table]),
	{ok, #state{roles_db=RoleDB}}.

%% ====================================================================
%% Server functions
%% ====================================================================

%% start_one_role(MapNode,MapProc,RoleId,IsAdult,Coord,GateNode,GateProc)->
%%	 %%to build a new connection to db when the role starts.
%%	 %%mysql_intf:new_pool(RoleId),
%%	 base_rpc_util:cast(MapNode, ?ROLE_MANAGER,{start_one_role,
%% 					MapProc,
%% 					RoleId,Coord,GateNode,GateProc}).

%% 启动一个角色进程:
%%	 地图信息(GS_system_map_info): 角色进程和指定地图进行绑定
%%	 角色信息(GS_system_role_info): 创建角色进程需要了解的基本信息
%%	 网关信息(GS_system_gate_info): 将角色进程和网关进程进行绑定
start_one_role(GS_system_map_info, GS_system_role_info, GS_system_gate_info,OtherInfo)->
	#gs_system_role_info{role_node=Role_node} = GS_system_role_info,
	base_rpc_util:cast(Role_node, ?ROLE_MANAGER, {start_one_role, {GS_system_map_info, GS_system_role_info, GS_system_gate_info,OtherInfo}}).
				
start_copy_role(GS_map_info, GS_role_info, GS_gate_info,X,Y,AllInfo)->	
	MapMgr_Node = get_node_from_mapinfo(GS_map_info),
	try
		?base_gen_server:call({?ROLE_MANAGER,MapMgr_Node}, {start_copy_role, {GS_map_info, GS_role_info, GS_gate_info,X,Y,AllInfo}},50000)
	catch
		E:R->
			base_logger_util:info_msg("start_copy_role ~p ~p  ~p ~n",[E,R,erlang:get_stacktrace()]),
			error
	end.		

stop_role_processor(RoleNode, RoleId, RolePid,Tag) when is_pid(RolePid) ->
	base_logger_util:info_msg("base_role_manager:stop_role_processor:~p,~p,~p~n", [RoleNode, RoleId, RolePid]),
	case base_role_processor:stop_role_processor(RolePid,Tag,RoleId) of
		{error,Reason}->
			{error,Reason};
		_->	
			base_role_sup:stop_role(RoleNode, RoleId)
	end;
	
stop_role_processor(RoleNode, RoleId, RoleProc,Tag) ->
	base_logger_util:info_msg("base_role_manager:stop_role_processor:~p,~p,~p~n", [RoleNode, RoleId, RoleProc]),
	case base_role_processor:stop_role_processor(RoleNode, RoleProc,Tag,RoleId) of
		{error,Reason}->
			{error,Reason};
		_->	
			base_role_sup:stop_role(RoleNode, RoleId)
	end.
	
%% 供给role process自己结束自己
stop_self_process(GS_map_info,RoleNode,RoleId)->
	base_rpc_util:cast(RoleNode, ?ROLE_MANAGER, {stop_role, {RoleNode,RoleId}}).
	

%% 获取和调用者处于同一Node的Role信息
get_role_info(undefined) ->
	base_logger_util:info_msg("undefined role info~n");

get_role_info(RoleId) ->
	try
		Role = ?base_ets:lookup(local_roles_datatbase, RoleId),
		case Role  of
			[] ->
				undefined;
			[{_, RoleInfo}] ->
				RoleInfo
		end
	catch
		_:_->
		base_logger_util:info_msg("get_role_info ets:lookup RoleId error:~p~n",[RoleId]),
		undefined
	end.

get_role_remote_info(RoleId)->
	try
		Role = ?base_ets:lookup(local_roles_datatbase, RoleId),
		case Role  of
			[] ->
				undefined;
			[{_, RoleInfo}] ->
				make_roleinfo_for_othernode(RoleInfo)
		end
	catch
		_:_->
		base_logger_util:info_msg("get_role_info ets:lookup RoleId error:~p~n",[RoleId]),
		undefined
	end.
	
%% 通过节点获取role信息
get_role_remoteinfo_by_node(Node,RoleId)->
	if
		Node =:= node()->
			get_role_remote_info(RoleId);
		true->
			try			 
				base_rpc_util:asyn_call(Node,?MODULE,get_role_remote_info,[RoleId])
			catch
				E:R-> base_logger_util:info_msg("get_role_info_by_node"),undefined
			end
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 向该节点的RoleManager,注册Role信息
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
regist_role_info(RoleId, RoleInfo) ->
	?base_ets:insert(local_roles_datatbase, {RoleId, RoleInfo}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 反注册角色信息TODO:时间
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unregist_role_info(RoleId) ->
	erlang:send_after(0, ?ROLE_MANAGER, {unregist_role_info,RoleId}).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 处理同步调用
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%{GS_map_info, GS_role_info, GS_gate_info,X,Y,AllInfo}
handle_call({start_copy_role,{_, GS_role_info,_,_,_,_}=AllCopyInfo},_From,State)->
	Role_id = get_id_from_roleinfo(GS_role_info),
	%% 检查属于该RoleId的processor是否存在
	case base_role_processor:whereis_role(Role_id) of
			undefined->
				nothing;
			_DeadProc->	%% 在本节点上已经有该进程,应该是僵死进程.直接关掉进程.
				base_role_sup:stop_role(node(), Role_id)
	end,
	Reply =			
	case base_role_sup:start_role({start_copy_role,AllCopyInfo}, Role_id) of
		{ok,_Role_pid} -> 					
			ok;
		AnyInfo ->
			base_logger_util:info_msg("base_role_manager:handle_info:start_copy_role:error:~p~n",[AnyInfo]),
			error			
	end,
	{reply, Reply, State};

handle_call(Request, From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 处理异步调用
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({regist_role_info, {Pid, RoleInfo}}, State) ->
	put(Pid, RoleInfo),	
	{noreply, State};

handle_cast(Msg, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 处理消息
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info({unregist_role_info,RoleId},State)->
	base_logger_util:info_msg("base_role_manager:unregist_role_info:~p~n",[RoleId]),
	?base_ets:delete(local_roles_datatbase, RoleId),
	{noreply, State};

%%GS_system_map_info, GS_system_role_info, GS_system_gate_info,OtherInfo
handle_info({start_one_role, {_, GS_system_role_info, GS_system_gate_info,_} = StartInfo},State)->
	%% 提取所需信息
	#gs_system_gate_info{gate_proc=Gate_proc}		 = GS_system_gate_info,
	#gs_system_role_info{role_id=Role_id}								  = GS_system_role_info,
	%% 检查帐号是否被封禁
	case gm_block_db:get_block_info(Role_id,login) of
		[]->
			BlockTime = -1;
		BlockInfo->
			StartTime = gm_block_db:get_start_time(BlockInfo),
			DurationTime = gm_block_db:get_duration_time(BlockInfo),
			LeftTime = erlang:trunc(DurationTime - (timer:now_diff(timer_center:get_correct_now(),StartTime) )/(1000*1000)),
			if
				DurationTime =:= 0->
					BlockTime = 0;
				LeftTime <0->
					BlockTime = -1,
					gm_block_db:delete_user(Role_id,login);
				true->
					BlockTime = LeftTime
			end
	end,
	if
		BlockTime =:= -1->
			case base_role_processor:whereis_role(Role_id) of
				undefined->
					nothing;
				_DeadProc->	%% 在本节点上已经有该进程,应该是僵死进程.直接关掉进程.
					base_logger_util:info_msg("start_one_role but has a processor,stop it ~n"),
					base_role_sup:stop_role(node(), Role_id)
			end,
			case base_role_sup:start_role({start_one_role,StartInfo}, Role_id) of
				{ok,_Role_pid} ->
					ok;
				AnyInfo ->
					Message = role_packet:encode_map_change_failed_s2c(?ERRNO_JOIN_MAP_ERROR_UNKNOWN),
					tcp_client:send_data(Gate_proc, Message),
					base_logger_util:info_msg("role_manager:handle_info:start_one_role:error:~p~n",[AnyInfo])
			end;
		true->	%% 发送角色封禁
			Message = role_packet:encode_block_s2c(?GM_BLOCK_TYPE_LOGIN,BlockTime),
			tcp_client:send_data(Gate_proc, Message)
	end,	
	{noreply, State};
	
handle_info({stop_role, {RoleNode,RoleId}},#state{roles_db = Role_db} = State)->
	base_role_sup:stop_role(RoleNode, RoleId),
	{noreply, State};
	
handle_info(Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
