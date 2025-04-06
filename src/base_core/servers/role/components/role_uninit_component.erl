%% Description: TODO: Add description to role_uninit_component
-module(role_uninit_component).
-export([
	handle_event/4,
	uninit/2,
	async_save_to_roledb/0
]).

-include("base_component_shared.hrl").
-include("role_struct.hrl").
-include("map_info_struct.hrl").
-include("little_garden.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 反初始化角色数据!!!组队转移队长,更新组队信息,更新副本信息
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
relocating_on_uninit()->
	%%位置修正
	case is_dead() of
		true->	
			case get_my_respwan_pos() of
				[]->
					nothing;
				{RespawnMapId,RespawnPos}->
					put(creature_info, set_pos_to_roleinfo(get(creature_info), RespawnPos)),
					put(map_info,set_mapid_to_mapinfo(get(map_info), RespawnMapId))
			end;
		_->
			nothing
	end,
%%	end,
	%%血量修正
	case is_dead() of
		true->	
			ModifyHp = erlang:trunc(get_hpmax_from_roleinfo(get(creature_info))*10/100), 			
			put(creature_info, set_state_to_roleinfo(get(creature_info), gaming)),
			put(creature_info, set_life_to_roleinfo(get(creature_info), ModifyHp ));		
		_->
			nothing
	end.

uninit(uninit,RoleId) ->
	try
		relocating_on_uninit()
	catch
		E:R-> base_logger_util:info_msg("base_role_op:uninit dead_on_uninit error ~p:~p ~p ~n",[E,R,erlang:get_stacktrace()])
	end,
	try
		group_op:hook_on_offline()			%%组队
	catch
		E1:R1-> base_logger_util:info_msg("base_role_op:uninit group_op:logout error ~p:~p ~p ~n",[E1,R1,erlang:get_stacktrace()])
	end,
	try
		guild_op:on_leave()			%%公会
	catch
		E2:R2-> base_logger_util:info_msg("base_role_op:uninit guild_op:on_leave error ~p:~p ~p ~n",[E2,R2,erlang:get_stacktrace()])
	end,
	try
		instance_op:on_offline()	%%副本
	catch
		E3:R3-> base_logger_util:info_msg("base_role_op:uninit instance_op:on_offline() error ~p:~p ~p ~n",[E3,R3,erlang:get_stacktrace()])
	end,
	try
		battle_ground_op:hook_on_offline()
	catch
		E31:R31-> base_logger_util:info_msg("base_role_op:uninit battle_ground_op:hook_on_offline error ~p:~p ~p ~n",[E31,R31,erlang:get_stacktrace()])
	end,	
	try
		friend_op:offline_notice()	%%好友
	catch
		E4:R4-> base_logger_util:info_msg("base_role_op:uninit friend_op:offline_notice() ~p:~p ~p ~n",[E4,R4,erlang:get_stacktrace()])
	end,
	try
		loop_tower_op:on_offline()	%%轮回塔
	catch
		E5:R5-> base_logger_util:info_msg("base_role_op:uninit loop_tower_op:on_offline() error ~p:~p ~p ~n",[E5,R5,erlang:get_stacktrace()])
	end,
	try
		series_kill:on_offline()
	catch
		E6:R6-> base_logger_util:info_msg("base_role_op:uninit series_kill:on_offline() error ~p:~p ~p ~n",[E6,R6,erlang:get_stacktrace()])
	end,
	try		
		timelimit_gift_op:on_playeroffline()
	catch
		E8:R8-> base_logger_util:info_msg("base_role_op:uninit timelimit_gift_op:on_playeroffline() error ~p:~p ~p ~n",[E8,R8,erlang:get_stacktrace()])
	end,
	try
		answer_op:hook_on_offline()	%%答题
	catch
		E9:R9-> base_logger_util:info_msg("base_role_op:uninit answer_op:hook_on_offline() error ~p:~p ~p ~n",[E9,R9,erlang:get_stacktrace()])
	end,
	try
		congratulations_op:hook_on_offline()	%%新手祝贺
	catch
		E10:R10-> base_logger_util:info_msg("base_role_op:uninit congratulations_op:hook_on_offline() error ~p:~p ~p ~n",[E10,R10,erlang:get_stacktrace()])
	end,
	try
		fatigue:on_playeroffline()
	catch
		E11:R11-> base_logger_util:info_msg("base_role_op:uninit fatigue:on_playeroffline() error ~p:~p ~p ~n",[E11,R11,erlang:get_stacktrace()])
	end,
	try
		offline_exp_op:hook_on_offline()	%%离线经验
	catch
		E12:R12-> base_logger_util:info_msg("base_role_op:uninit offline_exp_op:hook_on_offline() error ~p:~p ~p ~n",[E12,R12,erlang:get_stacktrace()])
	end,
	try
		venation_op:hook_on_offline()	    %%经脉
	catch
		E13:R13-> base_logger_util:info_msg("base_role_op:uninit venation_op:hook_on_offline error ~p:~p ~p ~n",[E13,R13,erlang:get_stacktrace()])
	end,
	try
		continuous_logging_op:on_player_offline()	   
	catch
		E14:R14-> base_logger_util:info_msg("base_role_op:uninit continuous_logging_op:on_player_offline error ~p:~p ~p ~n",[E14,R14,erlang:get_stacktrace()])
	end,
	try
		role_game_rank:on_player_offline()	   
	catch
		E15:R15-> base_logger_util:info_msg("base_role_op:uninit role_game_rank:on_player_offline error ~p:~p ~p ~n",[E15,R15,erlang:get_stacktrace()])
	end,
	try			%%元宝消耗兑换礼券
		gold_exchange:hook_on_offline()
	catch
		E17:R17-> base_logger_util:info_msg("base_role_op:uninit gold_exchage:hook_on_offline ~p:~p ~p ~n",[E17,R17,erlang:get_stacktrace()])
	end,
	try
		role_treasure_transport:hook_on_offline()
	catch
		E18:R18-> base_logger_util:info_msg("base_role_op:uninit role_treasure_transport:hook_on_offline ~p:~p ~p ~n",[E18,R18,erlang:get_stacktrace()])
	end,
	try			%%元宝消耗兑换物品
		consume_return:hook_on_offline()
	catch
		E19:R19-> base_logger_util:info_msg("base_role_op:uninit consume_return:hook_on_offline ~p:~p ~p ~n",[E19,R19,erlang:get_stacktrace()])
	end,
	try			
		spa_op:hook_on_offline()
	catch
		E20:R20-> base_logger_util:info_msg("base_role_op:uninit spa_op:hook_on_offline() ~p:~p ~p ~n",[E20,R20,erlang:get_stacktrace()])
	end,	
	try			%%主线
		role_mainline:uninit()
	catch
		Emainline:Rmainline-> base_logger_util:info_msg("base_role_op:uninit role_mainline:uninit ~p:~p ~p ~n",[Emainline,Rmainline,erlang:get_stacktrace()])
	end,
	try	
		guildbattle_op:hook_offline()
	catch
		Eguildbattle:Rguildbattle-> base_logger_util:info_msg("base_role_op:uninit guildbattle_op:hook_offline() ~p:~p ~p ~n",[Eguildbattle,Rguildbattle,erlang:get_stacktrace()])
	end,
	try	
		loop_instance_op:uninit()
	catch
		ELoopInstance:RLoopInstance-> base_logger_util:info_msg("base_role_op:uninit loop_Instance_op:uninit() ~p:~p ~p ~n",[ELoopInstance,RLoopInstance,erlang:get_stacktrace()])
	end,
	uninit(change_map,RoleId),
	OnlineTIME = trunc(timer:now_diff(base_timer_server:get_correct_now(),get(login_time))/1000000),	
	gm_logger_role:role_logout(RoleId,get(client_ip),OnlineTIME,get(level));
uninit(change_map,RoleId) ->
	async_save_to_roledb(),
	dmp_op:flush_bundle(RoleId),
	gm_logger_role:role_flush_items(RoleId,get(items_info)),
	trade_role:interrupt(),
	role_private_option:flush(),
	mall_op:save_to_db(),
	open_service_activities:on_player_off_line(),
	vip_op:hook_on_offline(),
	goals_op:save_to_db().
	%%fatigue:on_playeroffline().


handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.

is_dead()->
	creature_op:is_creature_dead(get(creature_info)).

%%
%%获取复活点位置
%%
get_my_respwan_pos()->
	case battle_ground_op:is_in_battle_ground() of
		true->
			case battle_ground_op:get_my_spawnpos() of
				[]->
					{get_mapid_from_mapinfo(get(map_info)),get_pos_from_roleinfo(get(creature_info))};
				RespawnPos->
					RespawnPos
			end;
		_->
			case guildbattle_op:is_in_battle() of
				true->
					{get_mapid_from_mapinfo(get(map_info)),guildbattle_op:get_my_bornpos()};
				_->
					case pvp_op:should_respawn_in_prison() of
						true->
							{?PRISON_MAP,?PRISON_POS};
						false->
							case mapop:get_respawn_pos(get(map_db)) of
								[]->	%%没有加载地图文件
									{get_mapid_from_mapinfo(get(map_info)),get_pos_from_roleinfo(get(creature_info))};
								DefaultRespawnPos->
									DefaultRespawnPos
							end
					end
			end	
	end.

async_save_to_roledb()->
	{A,B,C}=base_timer_server:get_correct_now(),%%%%%%%%%@@wb20130604 测试压力，临时修改
	put(db_save_time,{A,B+rand:uniform(10),C}),%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 	put(db_save_time,base_timer_server:get_correct_now()),
	async_write_to_roledb(),
	%%物品要同步写
	items_op:save_to_db(),
	quest_op:async_write_to_db(),
	skill_op:async_save_to_db(),
	instance_op:write_to_db(),
	pet_op:save_to_db().
	%%timelimit_gift_op:save_to_db().

async_write_to_roledb()->
	RoleInfo = get(creature_info),
	RoleId = get_id_from_roleinfo(RoleInfo),
	Pos = get_pos_from_roleinfo(RoleInfo),
	Silver = get_silver_from_roleinfo(RoleInfo),
	BoundSilver = get_boundsilver_from_roleinfo(RoleInfo),
	Level = get_level_from_roleinfo(RoleInfo),
	Gold = get_gold_from_roleinfo(RoleInfo),
	Gift = get_ticket_from_roleinfo(RoleInfo),
	Exp = get_exp_from_roleinfo(RoleInfo),
	Hp = get_life_from_roleinfo(RoleInfo),
	Mana = get_mana_from_roleinfo(RoleInfo),
	Mapid = get_mapid_from_mapinfo(get(map_info)),
	Packagesize = package_op:get_size(),	
	Bufflist = buffer_op:export_for_db(),
	GroupId = group_op:get_id(),
	GuildId = guild_util:get_guild_id(),
	TrainInfo = block_training_op:export_for_db(),
	Account = get(account_id),
	Name = get_name_from_roleinfo(RoleInfo),
	Sex = get_gender_from_roleinfo(RoleInfo),
	Class = get_class_from_roleinfo(RoleInfo),
	Honor = get_honor_from_roleinfo(RoleInfo),
	FightForce = get_fighting_force_from_roleinfo(RoleInfo),
	PvPInfo = {get_pkmodel_from_roleinfo(RoleInfo),get_crime_from_roleinfo(RoleInfo)},
	Pet = pet_op:save_roleinfo_to_db(),
	Offline = base_timer_server:get_correct_now(),
	SoulPower = role_soulpower:export_for_db(),
	StallName = auction_op:export_for_db(),
	base_role_db:async_write_roleattr({RoleId,Account,Name,Sex,Class,Level,Exp,Hp,Mana,Gold,Gift,Silver,BoundSilver,Mapid,Pos,Bufflist,TrainInfo,Packagesize,GroupId,GuildId,PvPInfo,Pet,Offline,SoulPower,StallName,Honor,FightForce}).
	