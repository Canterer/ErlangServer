-module(base_gate_op).

-export([
	get_role_list/2,
	get_last_mapid/1,
	create_role/4,
	create_role/14,
	get_socket_peer/1,
	trans_addr_to_list/1,
	check_socket/1,
	update_account_info/8
]).

-include("base_define_min.hrl").
-include("mnesia_table_def.hrl").
-include("error_msg.hrl").
-include("common_define.hrl").

get_role_list(AccountName,ServerId)->
	?base_logger_util:info_msg("get_role_list AccountName:~p ServerId:~p",[AccountName,ServerId]),
	AllRoles = base_role_db:get_role_list_by_account_rpc(AccountName),
	lists:filter(fun(LoginRole)->
						RoleId = base_pb_util:get_role_id_from_logininfo(LoginRole),
						ServerId =:= base_temp_util:get_serverid_by_roleid(RoleId)
				end,AllRoles).
	
get_last_mapid(RoleId) ->
	case base_role_db:get_role_info(RoleId) of
		[]-> 100;			%%born map
		RoleInfo->
			base_role_db:get_mapid(RoleInfo)
	end.
%%
%%

create_role(AccountId,AccountName,NickName,QQGender,RoleName,Gender,ClassType,CreateIp,ServerId,LoginTime,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level,Pf)->
	RegisterSwitch = base_env_ets:get(register_enable,?REGISTER_ENABLE),
	RoleNum = length(get_role_list(AccountName,ServerId)),
	if
		RoleNum >= 1 ->
			?base_logger_util:info_msg("account ~p ~p one role exist ~n",[AccountId,AccountName]),
			{failed,?ERR_CODE_CREATE_ROLE_EXISTED};
		RegisterSwitch =:= ?REGISTER_ENABLE ->
			case senswords_util:word_is_sensitive(RoleName)of
				false-> 
					case base_role_db:create_role_rpc(AccountId,AccountName,RoleName,Gender,ClassType,CreateIp,ServerId) of
						{ok,Result}->
							case base_db_dal_util:read_rpc(account,AccountName) of
								{ok,[AccountInfo]}->
									?ZSS(),
									#account{roleids = OldRoleIds} = AccountInfo,
									NewAccount = AccountInfo#account{roleids = [Result|OldRoleIds],is_yellow_vip=Is_yellow_vip,
																		is_yellow_year_vip = Is_yellow_year_vip, yellow_vip_level = Yellow_vip_level,
																		nickname = NickName, gender = QQGender, login_platform = Pf};
								_->
									?ZSS(),
									NewAccount = #account{username=AccountName,roleids=[Result],gold=0,first_login_ip = CreateIp,first_login_time = LoginTime,
															is_yellow_year_vip = Is_yellow_year_vip, first_login_platform = Pf, login_platform = Pf,
															yellow_vip_level = Yellow_vip_level,login_days = 0,nickname = NickName, gender = QQGender,
															qq_gold=0,local_gold=0}
							end,
							base_db_dal_util:write_rpc(NewAccount),
							{ok,Result};
						{failed,Reason}-> {failed,Reason};
							_Any->{failed,?ERR_CODE_CREATE_ROLE_INTERL}
					end;
				_-> ?base_logger_util:info_msg("senswords:word_is_sensitive :failed~n"),
					{failed,?ERR_CODE_ROLENAME_INVALID}
			end;
		true->
			{failed,?ERR_CODE_CREATE_ROLE_REGISTER_DISABLE}	
	end.

create_role(AccountId,AccountName,CreateIp,ServerId)->
	RoleName = binary_to_list(<<"游客">>)++ base_temp_util:make_int_str3(AccountId),
	Gender = rand:uniform(2)-1,
	ClassType = rand:uniform(3),
	RegisterSwitch = base_env_ets:get(register_enable,?REGISTER_ENABLE),
	if
		RegisterSwitch =:= ?REGISTER_ENABLE ->
			case base_role_db:create_role_rpc(AccountId,AccountName,{visitor,RoleName},Gender,ClassType,CreateIp,ServerId) of
				{ok,Result}->{ok,Result};
				{failed,Reason}-> {failed,Reason};
				_Any->{failed,?ERR_CODE_CREATE_ROLE_INTERL}
			end;
		true->
			{failed,?ERR_CODE_CREATE_ROLE_REGISTER_DISABLE}
	end.

get_socket_peer(Socket)->
	case inet:peername(Socket) of
		{error, _ } -> [];
		{ok,{Address,_Port}}->
			{A1,A2,A3,A4}= Address,
			string:join([integer_to_list(A1),
						 integer_to_list(A2),
						 integer_to_list(A3),
						 integer_to_list(A4)], ".")
	end.
	
trans_addr_to_list({A1,A2,A3,A4})->
	string:join([integer_to_list(A1),integer_to_list(A2),integer_to_list(A3),integer_to_list(A4)], ".").
	
check_socket(Socket)->
	case get_socket_peer(Socket) of
		[]-> false;
		IpAddress->
			% Ret = gm_block_db:check_block_info(IpAddress,connect),
			% if Ret >=0 -> false;
			%    true-> true
			% end
			true
	end.

update_account_info(AccountName, LoginTime, LoginIp, NickName, QQGender, Pf, Is_yellow_year_vip, Yellow_vip_level) ->
	case base_db_dal_util:read_rpc(account,AccountName) of
		{ok,[AccountInfo]}->
			#account{login_days = LoginDays} = AccountInfo,
			NewAccount = AccountInfo#account{last_login_ip = LoginIp, last_login_time = LoginTime, login_days = LoginDays + 1,
												is_yellow_year_vip = Is_yellow_year_vip,login_platform = Pf,
												yellow_vip_level = Yellow_vip_level,nickname = NickName, gender = QQGender},
			base_db_dal_util:write_rpc(NewAccount);
		_->
			?base_logger_util:info_msg("update_login_time_and_ip, error, AccountName, LoginTime, LoginIp: ~p, ~p, ~p~n", [AccountName, LoginTime, LoginIp])
	end.
