-module(role_instance_db).

-export([
	get_role_instance_info/1,
	save_role_instance_info/5,
	async_save_role_instance_info/5,
	get_instanceid/1,
	get_lastpostion/1,
	get_starttime/1,
	get_log/1
]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
% -define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
-include("mnesia_table_def.hrl").
%% --------------------------------------------------------------------
%%% behaviour functions begine
%% --------------------------------------------------------------------
?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	nothing.

?create_mnesia_split_table(role_instance,TrueTabName)->
	base_db_tools:create_table_disc(TrueTabName,record_info(fields,role_instance),[],set).

?delete_role_from_db(RoleId)->
	TableName = base_db_split_util:get_owner_table(role_instance, RoleId),
	base_db_dal_util:delete_rpc(TableName, RoleId).

?tables_info()->
	[{role_instance,disc_split}].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_role_instance_info(RoleId)->
	TableName = base_db_split_util:get_owner_table(role_instance, RoleId),
	case base_db_dal_util:read_rpc(TableName,RoleId) of
		{ok,[R]}-> R;
		{ok,[]}->[];
		{failed,badrpc,_Reason}->{TableName,RoleId,[]};
		{failed,_Reason}-> {TableName,RoleId,[]}
	end.
	
save_role_instance_info(RoleId,StartTime,InstanceId,LastPos,Log)->
	TableName = base_db_split_util:get_owner_table(role_instance, RoleId),
	base_db_dmp_op:sync_write(RoleId,{TableName,RoleId,StartTime,InstanceId,LastPos,Log}).

async_save_role_instance_info(RoleId,StartTime,InstanceId,LastPos,Log)->
	TableName = base_db_split_util:get_owner_table(role_instance, RoleId),
	base_db_dmp_op:async_write(RoleId,{TableName,RoleId,StartTime,InstanceId,LastPos,Log}).

get_instanceid(RoleInstanceInfo)->
	case RoleInstanceInfo of
		[]->[];
		_->
			erlang:element(#role_instance.instanceid, RoleInstanceInfo)
	end.
	
get_lastpostion(RoleInstanceInfo)->
	case RoleInstanceInfo of
		[]->[];
		_->
			erlang:element(#role_instance.lastpostion, RoleInstanceInfo)
	end.

get_starttime(RoleInstanceInfo)->
	case RoleInstanceInfo of
		[]->[];
		_->
			erlang:element(#role_instance.starttime, RoleInstanceInfo)
	end.

get_log(RoleInstanceInfo)->
	case RoleInstanceInfo of
		[]->[];
		_->
			erlang:element(#role_instance.log, RoleInstanceInfo)
	end.