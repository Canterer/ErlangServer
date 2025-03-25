%% Description: TODO: Add description to skill_db
-module(skill_db).

%%
%% Include files
%%
%%
%% Exported Functions
%%
-export([get_role_skill_info/1,get_skill_info/2,change_role_skill_level/3,save_role_skill_info/1,is_skill_studied/2,get_skill_level/2,
		get_skill_casttime/2,set_skill_casttime/2,add_new_skill/2,get_id/1,get_level/1,
		 get_name/1,get_type/1,get_rate/1,get_target_type/1,get_max_distance/1,get_isaoe/1,get_aoeradius/1,
		 get_interrupt/1,get_aoe_max_target/1,get_target_destroy/1,get_aoe_target_destroy/1,
		 get_self_destroy/1,get_cooldown/1,get_cast_type/1,get_cast_time/1,get_addtion_threat/1,
		 get_target_buff/1,get_caster_buff/1,get_remove_buff/1,
		 get_cost/1,get_flyspeed/1,get_learn_level/1, get_class/1,get_script/1,get_money/1,get_required_skills/1,get_quick_bar/1,update_quick_bar/4,
		async_save_role_skill_info/1,get_hit_addition/1,get_soulpower/1,get_creature/1,get_items/1,get_addtion_power/1,get_pet_skill_info/2
		]).
-export([get_pet_skill_buff/1]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
-define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
-define(SKILLS_TABLE_ETS,skill_table_ets).
-define(PET_SKILLS_ETS,pet_skills_ets).
-include("mnesia_table_def.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?create_ets()->
	?base_ets:new(?SKILLS_TABLE_ETS, [set,named_table]),
	?base_ets:new(?PET_SKILLS_ETS, [set,named_table]).

?init_ets()->
	db_operater_behaviour:init_ets(skills, ?SKILLS_TABLE_ETS,[#skills.id,#skills.level]),
	db_operater_behaviour:init_ets(pet_skill_template, ?PET_SKILLS_ETS, [#pet_skill_template.skillid,#pet_skill_template.skilllevel]).

?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	base_db_tools:create_table_disc(skills,record_info(fields,skills),[],bag),
	base_db_tools:create_table_disc(pet_skill_template, record_info(fields,pet_skill_template),[],bag).
?create_mnesia_split_table(role_skill,TrueTabName)->
	base_db_tools:create_table_disc(TrueTabName,record_info(fields,role_skill),[],set);
?create_mnesia_split_table(role_quick_bar,TrueTabName)->
 	base_db_tools:create_table_disc(TrueTabName,record_info(fields,role_quick_bar),[],set).

?delete_role_from_db(RoleId)->
	TableName1 = base_db_split_util:get_owner_table(role_skill, RoleId),
	base_db_dal_util:delete_rpc(TableName1, RoleId),
	TableName2 = base_db_split_util:get_owner_table(role_quick_bar, RoleId),
	base_db_dal_util:delete_rpc(TableName2, RoleId).

?tables_info()->
	[{role_skill,disc_split},{role_quick_bar,disc_split},{skills,proto}].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_role_skill_info(RoleId)->
	TableName = base_db_split_util:get_owner_table(role_skill, RoleId),
	case base_db_dal_util:read_rpc(TableName,RoleId) of
		{ok,[R]}-> R;
		{ok,[]}->{TableName,RoleId,[]};
		{failed,badrpc,_Reason}->{TableName,RoleId,[]};
		{failed,_Reason}-> {TableName,RoleId,[]}
	end.

save_role_skill_info(RoleSkillInfo)->
	RoleId = erlang:element(#role_skill.roleid, RoleSkillInfo),
	dmp_op:sync_write(RoleId,RoleSkillInfo).

async_save_role_skill_info(RoleSkillInfo)->
	RoleId = erlang:element(#role_skill.roleid, RoleSkillInfo),
	dmp_op:async_write(RoleId,RoleSkillInfo).

%%
%% return ->{TableName,RoleId,[{SlotId,ClassId,ObjectId}]}
%%
get_quick_bar(RoleId)->
	TableName = base_db_split_util:get_owner_table(role_quick_bar, RoleId),
	case base_db_dal_util:read_rpc(TableName,RoleId) of
		{ok,[R]}-> erlang:element(#role_quick_bar.quickbarinfo, R);
		{ok,[]}-> [];
		{failed,badrpc,_Reason}->[];
		{failed,_Reason}->[]
	end.

update_quick_bar(RoleId, SlotId, ClassId, EntryId)->
	TableName = base_db_split_util:get_owner_table(role_quick_bar, RoleId),
	dmp_op:async_write( RoleId, TableName,RoleId,#role_quick_bar.quickbarinfo, SlotId, {SlotId,ClassId,EntryId}).

%%
%% return : SkillInfo 
%%
get_skill_info(SkillId,Level)->
	case ?base_ets:lookup(?SKILLS_TABLE_ETS, {SkillId,Level}) of
		[]-> [];
		[{_,SkillInfo}]-> SkillInfo
	end.

get_pet_skill_info(SkillId,Level)->
	try
		case ?base_ets:lookup(?PET_SKILLS_ETS, {SkillId,Level}) of
			[]->[];
			[{_,Info}]->
				Info
		end
	catch
		_:_->
			io:format("lookup pet skill from ets error~n",[]),
			[]
	end.
%%
%% is_skill_studied ( RoleSkillInfo , {SkillId , Level } ) 
%%
%% SkillInfo -> return by get_role_skill_info()
%%
%% SkillId -> ClassId of Skill
%%--------------------------------------------------------------
%% is_skill_studied ( RoleSkillInfo ,SkillId ) 
%%
%% SkillInfo -> return by get_role_skill_info()
%%
%% SkillId -> ClassId of Skill
is_skill_studied(RoleSkillInfo,{SkillId,Level})->
	case SkillId of
		0-> false;
		_->
			case Level of
				0-> false;
				_->
					SkillList = element(#role_skill.skillinfo,RoleSkillInfo),
					case lists:filter(fun({Id,Lvl,_LastCast})-> (SkillId== Id) and (Level== Lvl) end, SkillList) of
						[]-> false;
						_-> true
					end
			end
	end;

is_skill_studied(RoleSkillInfo,SkillId)->
	case SkillId of
		0-> false;
		_->
			SkillList = element(#role_skill.skillinfo,RoleSkillInfo),
			lists:keymember(SkillId,1, SkillList)				
	end.

add_new_skill(RoleSkillInfo,SkillId)->
	SkillList = element(#role_skill.skillinfo,RoleSkillInfo),
	?base_erlang:setelement(#role_skill.skillinfo, RoleSkillInfo, SkillList ++ [{SkillId,1,{0,0,0}}]).

%% get_skill_level ( RoleSkillInfo , SkillId)
%%
%% SkillInfo -> return by get_role_skill_info()
%%
%% SkillId -> ClassId of Skill
get_skill_level(RoleSkillInfo,SkillId)->
	case SkillId of
		0-> 0;
		_->
			SkillList = element(#role_skill.skillinfo,RoleSkillInfo),
			case lists:keyfind(SkillId,1, SkillList) of
				false-> 0;
				{_,Level,_} -> Level
			end
	end.


change_role_skill_level(RoleSkillInfo,SkillId,LevelValue)->
	case SkillId of
		0-> 0;
		_->
			SkillList = element(#role_skill.skillinfo,RoleSkillInfo),
			NewSkillList = lists:map(fun({Skill,SkillLevel,LastCast})->
											if Skill =:=SkillId -> {Skill,LevelValue,LastCast} ;
											   true -> {Skill,SkillLevel,LastCast}
											end
									end, SkillList),
			?base_erlang:setelement(#role_skill.skillinfo, RoleSkillInfo, NewSkillList)
	end.
	
get_skill_casttime(RoleSkillInfo,SkillId)->
	case SkillId of
		0-> timer_center:get_correct_now();
		_->
			SkillList = element(#role_skill.skillinfo,RoleSkillInfo),
			case lists:keyfind(SkillId,1, SkillList) of
				false-> {0,0,0};
				{_,_,CastTime}-> CastTime
			end
	end.	

set_skill_casttime(RoleSkillInfo,SkillId)->
	SkillList = element(#role_skill.skillinfo,RoleSkillInfo),
	case lists:keyfind(SkillId,1, SkillList) of
		false-> RoleSkillInfo;
		{_,Level,_}-> 					
			NewRoleSkill = lists:keyreplace(SkillId,1,SkillList,{SkillId,Level,timer_center:get_correct_now()}),
			setelement(#role_skill.skillinfo,RoleSkillInfo,NewRoleSkill)
	end.	
	
%% @ get skill name
%%
%% get_name ( SkillInfo )
%%
%% SkillInfo -> return by get_skill_info()
%%
get_id(SkillInfo)->
	erlang:element(#skills.id, SkillInfo).

get_level(SkillInfo)->
	erlang:element(#skills.level, SkillInfo).

get_name(SkillInfo)->
	erlang:element(#skills.name, SkillInfo).

%% @ get skill descrition
%%
%% get_descrition ( SkillInfo )
%%
%% SkillInfo -> return by get_skill_info()
%%
get_type(SkillInfo)->
	erlang:element(#skills.type, SkillInfo).

get_rate(SkillInfo)->
	erlang:element(#skills.rate, SkillInfo).

get_target_type(SkillInfo)->
	erlang:element(#skills.target_type, SkillInfo).

get_max_distance(SkillInfo)->
	erlang:element(#skills.max_distance, SkillInfo).

get_isaoe(SkillInfo)->
	erlang:element(#skills.isaoe, SkillInfo).

get_aoeradius(SkillInfo)->
	erlang:element(#skills.aoeradius, SkillInfo).

get_interrupt(SkillInfo)->
	erlang:element(#skills.interrupt, SkillInfo).

get_aoe_max_target(SkillInfo)->
	erlang:element(#skills.aoe_max_target, SkillInfo).

get_target_destroy(SkillInfo)->
	erlang:element(#skills.target_destroy, SkillInfo).

get_aoe_target_destroy(SkillInfo)->
	erlang:element(#skills.aoe_target_destroy, SkillInfo).


get_self_destroy(SkillInfo)->
	erlang:element(#skills.self_destroy, SkillInfo).

get_cooldown(SkillInfo)->
	erlang:element(#skills.cooldown, SkillInfo).

get_cast_type(SkillInfo)->
	erlang:element(#skills.cast_type, SkillInfo).

get_cast_time(SkillInfo)->
	erlang:element(#skills.cast_time, SkillInfo).

get_addtion_threat(SkillInfo)->
	erlang:element(#skills.addtion_threat, SkillInfo).

%%
%% return list:[] | [BufferId]
%%
get_target_buff(SkillInfo)->
	erlang:element(#skills.target_buff, SkillInfo).

%%
%% return list:[] | [BufferId]
%%
get_caster_buff(SkillInfo)->
	erlang:element(#skills.caster_buff, SkillInfo).
%%
%% return list:[] | [BufferId]
%%
get_remove_buff(SkillInfo)->
	erlang:element(#skills.caster_buff, SkillInfo).

get_cost(SkillInfo)->
	erlang:element(#skills.cost, SkillInfo).
	
get_flyspeed(SkillInfo)->
	erlang:element(#skills.flyspeed, SkillInfo).
	
get_learn_level(SkillInfo)->
	erlang:element(#skills.learn_level, SkillInfo).

get_class(SkillInfo)->
	erlang:element(#skills.class, SkillInfo).

get_script(SkillInfo)->
	erlang:element(#skills.script, SkillInfo).
	
get_money(SkillInfo)->
	erlang:element(#skills.money, SkillInfo).

get_required_skills(SkillInfo)->
	erlang:element(#skills.required_skills, SkillInfo).

get_hit_addition(SkillInfo)->
	erlang:element(#skills.hit_addition, SkillInfo).

get_soulpower(SkillInfo)->
	erlang:element(#skills.soulpower, SkillInfo).
  
get_creature(SkillInfo)->
	erlang:element(#skills.creature, SkillInfo).

get_items(SkillInfo)->
	erlang:element(#skills.items, SkillInfo).

get_addtion_power(SkillInfo)->
	erlang:element(#skills.addtion_power, SkillInfo).

get_pet_skill_buff(SkillInfo)->
	erlang:element(#pet_skill_template.skill, SkillInfo).