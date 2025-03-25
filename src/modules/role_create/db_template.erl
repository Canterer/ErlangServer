%% Description: TODO: Add description to db_template
-module(db_template).

%%
%% Exported Functions
%%
-export([
	create_template_role/4,
	import/1
]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
% -define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
-include("mnesia_table_def.hrl").
-include("role_template.hrl").
%%
%% behaviour functions
%%
?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	base_db_tools:create_table_disc(template_roleattr,record_info(fields,roleattr),[account,name],set),
	base_db_tools:create_table_disc(template_role_quick_bar,record_info(fields,role_quick_bar),[],set),
	base_db_tools:create_table_disc(template_role_skill,record_info(fields,role_skill),[],set),
	base_db_tools:create_table_disc(template_playeritems,record_info(fields,playeritems),[ownerguid],set),
	base_db_tools:create_table_disc(template_itemproto, record_info(fields,template_itemproto), [], set),
	base_db_tools:create_table_disc(template_quest_role,record_info(fields,quest_role),[],set).

?create_mnesia_split_table(_,_)->
	nothing.

?delete_role_from_db(_)->
	nothing.

?tables_info()->
	[{template_roleattr,proto},{template_role_quick_bar,proto},{template_role_skill,proto},{template_playeritems,proto},{template_itemproto,proto},{template_quest_role,proto}].
%%
%% Local Functions
%%

create_template_role(TemplateId,RoleName,Account,ServerId)->
	?base_logger_util:info_msg("db_template create_template_role TemplateId:~p, RoleName:~p, Account:~p, ServerId:~p ~n",[TemplateId,RoleName,Account,ServerId]),
	try
		RoleId = base_role_id_generator_server:gen_newid(ServerId),
		RoleTable = base_db_split_util:get_owner_table(roleattr, RoleId), 
		QuickBarTable =  base_db_split_util:get_owner_table(role_quick_bar, RoleId),
		RoleSkillTable =  base_db_split_util:get_owner_table(role_skill, RoleId),
		PlayerItemsTable =  base_db_split_util:get_owner_table(playeritems, RoleId),
		QuestRoleTable = base_db_split_util:get_owner_table(quest_role, RoleId),
		?base_logger_util:info_msg("db_template create_template_role RoleId:~p, RoleTable:~p, QuickBarTable:~p~n",[RoleId,RoleTable,QuickBarTable]),
		RoleAttr = case get_roleattr(TemplateId) of
						{ok,[]}-> [];
						{ok,R}-> R
					end,
		RoleQuickBar = case get_role_quick_bar(TemplateId) of
						{ok,[]}-> [];
						{ok,Q}-> Q;
						{failed,Reason2}-> throw (Reason2)
					end,
		RoleSkill = case get_role_skill(TemplateId) of
						{ok,[]}-> [];
						{ok,S}-> S;
						{failed,Reason3}-> throw (Reason3)
					end,
		PlayerItems = case get_playeritems(TemplateId) of
						{ok,[]}-> [];
						{ok,P}-> P;
						{failed,Reason4}-> throw (Reason4)
					end,
		RoleQuests = case get_quest_role(TemplateId) of
						{ok,[]}-> [];
						{ok,QR}-> QR;
						{failed,Reason5}-> throw (Reason5)
					end,
		?ZSS("RoleTable:~p, RoleId:~p,RoleName:~p,Account:~p",[RoleTable,RoleId,role_name_to_presist(RoleName),Account]),
		[RoleAttr1|_] = RoleAttr,
		RoleAttr2 = ?base_erlang:setelement(1, RoleAttr1, RoleTable),
		RoleAttr3 = ?base_erlang:setelement(#roleattr.roleid, RoleAttr2, RoleId),
		RoleAttr4 = ?base_erlang:setelement(#roleattr.name, RoleAttr3, role_name_to_presist(RoleName)),
		RoleAttr5 = ?base_erlang:setelement(#roleattr.account, RoleAttr4,Account),
		FatigueList = base_env_ets:get2(fatigue, fatigue_list, []),
		NoFatigueList = base_env_ets:get2(fatigue, nofatigue_list, []),
		RoleAttr6 = case lists:filter(fun({AccountItem,_})->
								Account=:=AccountItem
						end , FatigueList ++ NoFatigueList) of
			[]-> RoleAttr5;
			[{_Account,Level}]->?base_erlang:setelement(#roleattr.level,RoleAttr5,Level);
			[{_Account,Level}|_T]->?base_erlang:setelement(#roleattr.level,RoleAttr5,Level)
		end,
			
		[RoleQuickBar1|_] = RoleQuickBar,
		RoleQuickBar2 = ?base_erlang:setelement(1, RoleQuickBar1, QuickBarTable),
		RoleQuickBar3 = ?base_erlang:setelement(#role_quick_bar.roleid, RoleQuickBar2, RoleId),
		[RoleSkil1|_] = RoleSkill,
		RoleSkil2 = ?base_erlang:setelement(1, RoleSkil1, RoleSkillTable),
		RoleSkil3 = ?base_erlang:setelement(#role_skill.roleid, RoleSkil2, RoleId),
		?ZSS("PlayerItems:~p",[PlayerItems]),
		?ZSS("PlayerItemsTable:~p",[PlayerItemsTable]),
		NewPlayerItems = lists:map(fun(PlayerItem)->
						PlayerItem1 = ?base_erlang:setelement(1,PlayerItem, PlayerItemsTable),
						PlayerItem2 = ?base_erlang:setelement(#playeritems.id, PlayerItem1,itemid_generator:gen_newid()),
						PlayerItem3 = ?base_erlang:setelement(#playeritems.ownerguid, PlayerItem2, RoleId),
						PlayerItemProtoId = erlang:element(#playeritems.entry,PlayerItem3),
						case get_item_proto(PlayerItemProtoId) of
							[]->
								PlayerItem4 = PlayerItem3;
							ItemProtoInfo->
								?ZSS(),
								OverDue = items_op:create_item_overdue(ItemProtoInfo),
								PlayerItem4 = ?base_erlang:setelement(#playeritems.overdueinfo, PlayerItem3, OverDue)
						end,
						PlayerItem4
					end, PlayerItems),
		[RoleQuests1|_]=RoleQuests,
		RoleQuests2 = ?base_erlang:setelement(1,RoleQuests1, QuestRoleTable),
		RoleQuests3 = ?base_erlang:setelement(#quest_role.roleid, RoleQuests2, RoleId),
		?base_logger_util:info_msg("~p:~p:line:~p (RoleAttr6=:~p,RoleQuickBar3=:~p,RoleSkil3=:~p,RoleQuests3=:~p,NewPlayerItems=:~p)",[?MODULE,?FUNCTION_NAME,?LINE,RoleAttr6,RoleQuickBar3,RoleSkil3,RoleQuests3,NewPlayerItems]),
		QF = fun()->
					?base_mnesia:write(RoleAttr6),
					?base_mnesia:write(RoleQuickBar3),
					?base_mnesia:write(RoleSkil3),
					lists:foreach(fun(X)-> ?base_mnesia:write(X) end, NewPlayerItems),
					?base_mnesia:write(RoleQuests3)
			end,
		case base_db_dal_util:run_transaction(QF) of
			{ok,_}-> {ok,RoleId};
			Error-> ?base_logger_util:info_msg("Error ~p ~n",[Error]),{failed}
		end
	catch
		throw : no_roleattr_template -> ?base_logger_util:info_msg("exception:no_roleattr_template~n"),{failed};
		throw : no_role_quick_bar_template ->?base_logger_util:info_msg("exception:no_role_quick_bar_template~n"), {failed};
		throw : no_role_skill_template -> ?base_logger_util:info_msg("exception:no_role_skill_template~n"),{failed};
		throw : no_playeritems_template ->?base_logger_util:info_msg("exception:no_playeritems_template~n"), {failed};
		throw : noquest_role_template ->?base_logger_util:info_msg("exception:noquest_role_template~n"), {failed};
		throw : Reason ->?base_logger_util:info_msg("exception:~p ~n",[Reason]), {failed};
		E:Re-> ?base_logger_util:info_msg("exception:~p ~p ~n",[E,Re]),{failed}
	end.

role_name_to_presist(RoleName) when is_list(RoleName)->
	list_to_binary(RoleName);
role_name_to_presist(RoleName) when is_binary(RoleName)->
	RoleName;
role_name_to_presist(RoleName) when is_tuple(RoleName)->
	case RoleName of
		{Key,RName}-> {Key,role_name_to_presist(RName)};
		_->RoleName
	end.

get_roleattr(TemplateId)->
	case base_db_dal_util:read(template_roleattr, TemplateId) of
		{ok,Result}->{ok,Result};
		_->{ok,[]}
	end.

get_role_quick_bar(TemplateId)->
	case base_db_dal_util:read(template_role_quick_bar, TemplateId) of
		{ok,Result}->{ok,Result};
		_->{ok,[]}
	end.

get_role_skill(TemplateId)->
	case base_db_dal_util:read(template_role_skill, TemplateId) of
		{ok,Result}->{ok,Result};
		_->{ok,[]}
	end.

get_playeritems(TemplateId)->
	case base_db_dal_util:read_index(template_playeritems, TemplateId, #playeritems.ownerguid) of
		{ok,Re} ->{ok,Re};
		_->{ok,[]}
	end.

get_quest_role(TemplateId)->
	case base_db_dal_util:read(template_quest_role, TemplateId) of
		{ok,Result}->{ok,Result};
		_->{ok,[]}
	end.

get_item_proto(TemplateId)->
	case base_db_dal_util:read(item_template, TemplateId) of
		{ok,[Result]}->Result;
		_->[]
	end.

import(File)->
	base_db_dal_util:clear_table(template_roleattr),
	base_db_dal_util:clear_table(template_role_skill),
	base_db_dal_util:clear_table(template_role_quick_bar),
	base_db_dal_util:clear_table(template_playeritems),
	base_db_dal_util:clear_table(template_quest_role),
	case file:consult(File) of
		{ok,Terms}->
			lists:foreach(fun(Term)->
								base_db_dal_util:write(Term)
						end,Terms);
		{error,Reason} ->
			?base_logger_util:info_msg("imort role template failed error:~p~n",[Reason])
	end.
