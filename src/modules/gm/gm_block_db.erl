-module(gm_block_db).
-include("mnesia_table_def.hrl").
-include("common_define.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([add_user/3,get_block_info/2,get_start_time/1,get_duration_time/1,delete_user/2,check_block_info/2]).
%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
% -define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").

?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	base_db_tools:create_table_disc(gm_blockade, record_info(fields,gm_blockade),[],set).

?create_mnesia_split_table(_,_)->
	nothing.

?delete_role_from_db(RoleId)->
	base_db_dal_util:delete_rpc(gm_blockade,{RoleId,talk}),
	base_db_dal_util:delete_rpc(gm_blockade,{RoleId,login}),
	base_db_dal_util:delete_rpc(gm_blockade,{RoleId,connect}).

?tables_info()->
	[{gm_blockade,disc}].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%DurationTime<=2147483647,forever DurationTime=0,
add_user(UserId,talk,DurationTime)->
	StartTime = base_timer_server:get_correct_now(),
	base_db_dal_util:write_rpc({gm_blockade,{UserId,talk},StartTime ,DurationTime});
	
add_user(UserId,login,DurationTime)->
	StartTime = base_timer_server:get_correct_now(),
	base_db_dal_util:write_rpc({gm_blockade,{UserId,login},StartTime ,DurationTime});

add_user(IpAddress,connect,DurationTime)->
	StartTime = base_timer_server:get_correct_now(),
	base_db_dal_util:write_rpc({gm_blockade,{IpAddress,connect},StartTime ,DurationTime}).

delete_user(UserId,login)->
	base_db_dal_util:delete_rpc(gm_blockade,{UserId,login});

delete_user(UserId,talk)->
	base_db_dal_util:delete_rpc(gm_blockade,{UserId,talk});

delete_user(IpAddress,connect)->
	base_db_dal_util:delete_rpc(gm_blockade,{IpAddress,connect}).
	
get_block_info(UserId,talk)->
	case base_db_dal_util:read_rpc(gm_blockade,{UserId,talk}) of
		{ok,[BlockInfo]}->
			BlockInfo;
		_->
			[]
	end;
	
get_block_info(UserId,login)->
	case base_db_dal_util:read_rpc(gm_blockade,{UserId,login}) of
		{ok,[BlockInfo]}->
			BlockInfo;
		_->
			[]
	end;

get_block_info(IpAddress,connect)->
	case base_db_dal_util:read_rpc(gm_blockade,{IpAddress,connect}) of
		{ok,[BlockInfo]}->
			BlockInfo;
		_->
			[]
	end.
	
get_start_time(BlockInfo)->
	erlang:element(#gm_blockade.start_time, BlockInfo).

get_duration_time(BlockInfo)->
	erlang:element(#gm_blockade.duration_time, BlockInfo).

check_block_info(ClientContext,BlockType)->
	case gm_block_db:get_block_info(ClientContext,BlockType) of
		[]->
			BlockTime = -1;
		BlockInfo->
			StartTime = gm_block_db:get_start_time(BlockInfo),
			DurationTime = gm_block_db:get_duration_time(BlockInfo),
			LeftTime = erlang:trunc(DurationTime - (timer:now_diff(base_timer_server:get_correct_now(),StartTime) )/(1000*1000)),
			if
				DurationTime =:= 0->
					BlockTime = 0;
				LeftTime <0->
					BlockTime = -1,
					gm_block_db:delete_user(ClientContext,BlockType);
				true->
					BlockTime = LeftTime
			end
	end.