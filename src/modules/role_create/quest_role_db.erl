%% Description: TODO: Add description to quest_role
-module(quest_role_db).
-include("mnesia_table_def.hrl").
%%
%% Include files
%%
%%
%% Exported Functions
%%
-export([
	get_questinfo_by_roleid/1,
	get_quest_list/1,
	async_update_quest_role/2,
	update_quest_role_now/2
]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
% -define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	nothing.

?create_mnesia_split_table(quest_role,TrueTabName)->
	base_db_tools:create_table_disc(TrueTabName,record_info(fields,quest_role),[],set).

?delete_role_from_db(RoleId)->
	OwnerTable = base_db_split_util:get_owner_table(quest_role, RoleId),
	base_db_dal_util:delete_rpc(OwnerTable, RoleId).

?tables_info()->
	[{quest_role,disc_split}].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% API Functions
%%
get_questinfo_by_roleid(RoleId)->
	TableName = base_db_split_util:get_owner_table(quest_role, RoleId),
	case base_db_dal_util:read_rpc(TableName,RoleId) of
		{ok,[]}-> {TableName,RoleId,[]};
		{ok,[Result]}-> Result;
		{failed,badrpc,Reason}-> base_logger_util:info_msg("get_questinfo_by_roleid failed ~p:~p~n",[badrpc,Reason]);
		{failed,Reason}-> base_logger_util:info_msg("get_questinfo_by_roleid failed :~p~n",[Reason])
	end.
        
get_quest_list(QuestRole)->
	try
		element(#quest_role.quest_list,QuestRole)
	catch
		_:_-> []
	end.

async_update_quest_role(RoleId,Quest_list)->	
	TableName = base_db_split_util:get_owner_table(quest_role, RoleId),
	dmp_op:async_write(RoleId,{TableName,RoleId,Quest_list}).

update_quest_role_now(RoleId,Quest_list)->
	TableName = base_db_split_util:get_owner_table(quest_role, RoleId),
	dmp_op:sync_write(RoleId,{TableName,RoleId,Quest_list}).
