%% Description: 生成配置文件数据库
-module(base_mnesia_gen_tables_util).
-include("base_define_min.hrl").

%%
%% Include files
%%
-include("mnesia_table_record_def.hrl").
%%
%% Exported Functions
%%
-export([run/0, read/2]).

%%
%% API Functions
%%

read(Table, Key) ->
	?base_mnesia:start(),
	?base_mnesia:wait_for_tables(?base_mnesia:system_info(tables), infinity),
	Result = ?base_mnesia:dirty_read(Table, Key),
	?base_logger_util:info_msg("~p~n", [Result]),
	?base_mnesia:stop().

run() ->
	filelib:ensure_dir("../log/"),
	error_logger:logfile({open, "../log/mnesia_gen_tables.log"}),
	?ZSS(),
	?base_mnesia:create_schema([node()]),
	?base_mnesia:start(),
	?base_mnesia:wait_for_tables(?base_mnesia:system_info(tables), infinity),
	delete_all_tables(),
	create_all_tables(),
	gen_game_db(),
	gen_creature_db(),
	?base_mnesia:stop(),
	?base_mnesia:start(),
	?base_mnesia:wait_for_tables(?base_mnesia:system_info(tables), infinity),
	?base_mnesia:stop(),
	?base_logger_util:info_msg("mnesia_gen_tables, process finished!!!~n").


create_all_tables() ->
	?ZSS(),
	Info = record_info(fields,achieve_proto),
	?ZSS(),
	?ZSS("info:~p",[Info]),
	base_db_tools:create_table_disc(achieve_proto, record_info(fields,achieve_proto), [], set),
	base_db_tools:create_table_disc(achieve_fuwen, record_info(fields,achieve_fuwen), [], set),
	base_db_tools:create_table_disc(achieve_award, record_info(fields,achieve_award), [], set),
	base_db_tools:create_table_disc(achieve, record_info(fields,achieve), [], set),
	base_db_tools:create_table_disc(activity,record_info(fields,activity),[],bag),
	base_db_tools:create_table_disc(activity_value_proto,record_info(fields,activity_value_proto),[],set),
	base_db_tools:create_table_disc(activity_value_reward,record_info(fields,activity_value_reward),[],set),
	base_db_tools:create_table_disc(ai_agents,record_info(fields,ai_agents),[],bag),
	base_db_tools:create_table_disc(answer,record_info(fields,answer),[],set),
	base_db_tools:create_table_disc(answer_option,record_info(fields,answer_option),[],set),
	base_db_tools:create_table_disc(attr_info,record_info(fields,attr_info),[],set),
	base_db_tools:create_table_disc(auto_name,record_info(fields,auto_name),[],set),
	base_db_tools:create_table_disc(back_echantment_stone,record_info(fields,back_echantment_stone),[],set),
	base_db_tools:create_table_disc(battlefield_proto,record_info(fields,battlefield_proto),[],set),
	base_db_tools:create_table_disc(block_training, record_info(fields,block_training), [], set),
	base_db_tools:create_table_disc(buffers,record_info(fields,buffers),[],bag),
	base_db_tools:create_table_disc(chat_condition,record_info(fields,chat_condition),[],set),
	base_db_tools:create_table_disc(chess_spirit_config,record_info(fields,chess_spirit_config),[],set),
	base_db_tools:create_table_disc(chess_spirit_rewards,record_info(fields,chess_spirit_rewards),[],set),
	base_db_tools:create_table_disc(chess_spirit_section,record_info(fields,chess_spirit_section),[],set),
	base_db_tools:create_table_disc(christmas_activity_reward,record_info(fields,christmas_activity_reward),[],set),
	base_db_tools:create_table_disc(christmas_tree_config,record_info(fields,christmas_tree_config),[],set),
	base_db_tools:create_table_disc(classbase,record_info(fields,classbase),[],bag),
	base_db_tools:create_table_disc(congratulations,record_info(fields,congratulations),[],set),
	base_db_tools:create_table_disc(continuous_logging_gift,record_info(fields,continuous_logging_gift),[],set),
	base_db_tools:create_table_disc(country_proto,record_info(fields,country_proto),[],set),
	base_db_tools:create_table_disc(creature_proto,record_info(fields,creature_proto),[],set),
	base_db_tools:create_table_disc(designation_data,record_info(fields,designation_data),[],set),
	base_db_tools:create_table_disc(dragon_fight_db,record_info(fields,dragon_fight_db),[],set),
	base_db_tools:create_table_disc(drop_rule,record_info(fields,drop_rule),[],set),
	base_db_tools:create_table_disc(enchantments, record_info(fields,enchantments), [], set),
	base_db_tools:create_table_disc(enchantments_lucky, record_info(fields,enchantments_lucky), [], set),
	base_db_tools:create_table_disc(enchant_convert,record_info(fields,enchant_convert),[],set),
	base_db_tools:create_table_disc(enchant_opt,record_info(fields,enchant_opt),[],set),
	base_db_tools:create_table_disc(enchant_property_opt,record_info(fields,enchant_property_opt),[],bag),
	base_db_tools:create_table_disc(equipmentset,record_info(fields,equipmentset),[],bag),
	base_db_tools:create_table_disc(equipment_fenjie, record_info(fields,equipment_fenjie), [], set),
	base_db_tools:create_table_disc(equipment_move, record_info(fields,equipment_move),[], bag),%%枫少修改bag
	base_db_tools:create_table_disc(equipment_sysbrd,record_info(fields,equipment_sysbrd),[],set),
	base_db_tools:create_table_disc(equipment_upgrade, record_info(fields,equipment_upgrade), [], set),
	base_db_tools:create_table_disc(everquests,record_info(fields,everquests),[],set),
	base_db_tools:create_table_disc(faction_relations,record_info(fields,faction_relations),[],set),
	base_db_tools:create_table_disc(festival_control,record_info(fields,festival_control),[],set),
	base_db_tools:create_table_disc(festival_recharge_gift,record_info(fields,festival_recharge_gift),[],set),
	base_db_tools:create_table_disc(goals, record_info(fields,goals), [], set),
	base_db_tools:create_table_disc(guild_authorities, record_info(fields,guild_authorities),[],set),
	base_db_tools:create_table_disc(guild_auth_groups, record_info(fields,guild_auth_groups), [], bag),
	base_db_tools:create_table_disc(guild_battle_proto,record_info(fields,guild_battle_proto),[],set),
	base_db_tools:create_table_disc(guild_facilities, record_info(fields,guild_facilities), [], bag),
	base_db_tools:create_table_disc(guild_monster_proto,record_info(fields,guild_monster_proto),[],set),
	base_db_tools:create_table_disc(guild_setting,record_info(fields,guild_setting),[],set),
	base_db_tools:create_table_disc(guild_shop,record_info(fields,guild_shop),[],set),
	base_db_tools:create_table_disc(guild_shop_items,record_info(fields,guild_shop_items),[],set),
	base_db_tools:create_table_disc(guild_treasure,record_info(fields,guild_treasure),[],set),
	base_db_tools:create_table_disc(guild_treasure_items,record_info(fields,guild_treasure_items),[],set),
	base_db_tools:create_table_disc(guild_treasure_transport_consume,record_info(fields,guild_treasure_transport_consume),[],set),
	base_db_tools:create_table_disc(honor_store_items, record_info(fields,honor_store_items), [], set),
	base_db_tools:create_table_disc(inlay, record_info(fields,inlay), [], set),
	base_db_tools:create_table_disc(instance_proto, record_info(fields,instance_proto), [], set),
	base_db_tools:create_table_disc(item_identify,record_info(fields,item_identify),[],set),
	base_db_tools:create_table_disc(item_template,record_info(fields,item_template),[],set),
	base_db_tools:create_table_disc(jszd_rank_option, record_info(fields,jszd_rank_option), [], set),
	base_db_tools:create_table_disc(levelup_opt,record_info(fields,levelup_opt),[],set),
	base_db_tools:create_table_disc(level_activity_rewards_db,record_info(fields,level_activity_rewards_db),[],set),
	base_db_tools:create_table_disc(loop_instance, record_info(fields,loop_instance), [], set),
	base_db_tools:create_table_disc(loop_instance_proto, record_info(fields,loop_instance_proto), [], set),
	base_db_tools:create_table_disc(loop_tower, record_info(fields,loop_tower), [], set),
	base_db_tools:create_table_disc(lottery_counts, record_info(fields,lottery_counts), [], set),
	base_db_tools:create_table_disc(lottery_droplist, record_info(fields,lottery_droplist), [], set),
	base_db_tools:create_table_disc(mainline_defend_config,record_info(fields,mainline_defend_config),[],bag),
	base_db_tools:create_table_disc(mainline_proto,record_info(fields,mainline_proto),[],bag),
	base_db_tools:create_table_disc(mall_item_info, record_info(fields,mall_item_info), [], set),
	base_db_tools:create_table_disc(mall_sales_item_info, record_info(fields,mall_sales_item_info), [], set),
	base_db_tools:create_table_disc(map_info, record_info(fields,map_info), [], set),
	base_db_tools:create_table_disc(npc_dragon_fight,record_info(fields,npc_dragon_fight),[],set),
	base_db_tools:create_table_disc(npc_drop,record_info(fields,npc_drop),[],set),
	base_db_tools:create_table_disc(npc_exchange_list,record_info(fields,npc_exchange_list),[],set),
	base_db_tools:create_table_disc(everquest_list,record_info(fields,everquest_list),[],set),
	base_db_tools:create_table_disc(npc_sell_list,record_info(fields,npc_sell_list),[],set),
	base_db_tools:create_table_disc(npc_trans_list,record_info(fields,npc_trans_list),[],set),
	base_db_tools:create_table_disc(quest_npc,record_info(fields,quest_npc),[],set),
	base_db_tools:create_table_disc(npc_functions,record_info(fields,npc_functions),[],set),
	base_db_tools:create_table_disc(offline_everquests_exp,record_info(fields,offline_everquests_exp),[],set),
	base_db_tools:create_table_disc(offline_exp,record_info(fields,offline_exp),[],set),
	base_db_tools:create_table_disc(open_service_activities,record_info(fields,open_service_activities),[],set),
	base_db_tools:create_table_disc(open_service_activities_time,record_info(fields,open_service_activities_time),[],set),
	base_db_tools:create_table_disc(pet_evolution,record_info(fields,pet_evolution),[],set),
	base_db_tools:create_table_disc(pet_explore_gain,record_info(fields,pet_explore_gain),[],set),
	base_db_tools:create_table_disc(pet_explore_style,record_info(fields,pet_explore_style),[],set),
	base_db_tools:create_table_disc(pet_growth,record_info(fields,pet_growth),[],set),
	base_db_tools:create_table_disc(pet_happiness,record_info(fields,pet_happiness),[],set),
	base_db_tools:create_table_disc(pet_level,record_info(fields,pet_level),[],set),
	base_db_tools:create_table_disc(pet_proto,record_info(fields,pet_proto),[],set),
	base_db_tools:create_table_disc(pet_quality,record_info(fields,pet_quality),[],set),
	base_db_tools:create_table_disc(pet_quality_up,record_info(fields,pet_quality_up),[],set),
	base_db_tools:create_table_disc(pet_skill_slot,record_info(fields,pet_skill_slot),[],set),
	base_db_tools:create_table_disc(pet_slot,record_info(fields,pet_slot),[],set),
	base_db_tools:create_table_disc(pet_talent_consume,record_info(fields,pet_talent_consume),[],set),
	base_db_tools:create_table_disc(pet_talent_rate,record_info(fields,pet_talent_rate),[],bag),
	base_db_tools:create_table_disc(pet_wash_attr_point,record_info(fields,pet_wash_attr_point),[],set),
	base_db_tools:create_table_disc(pet_item_mall,record_info(fields,pet_item_mall),[],set),%%宠物商店初始化信息表《枫少》
	base_db_tools:create_table_disc(quests,record_info(fields,quests),[],set),
	base_db_tools:create_table_disc(refine_system,record_info(fields,refine_system),[],set),
	base_db_tools:create_table_disc(remove_seal, record_info(fields,remove_seal), [], set),
	base_db_tools:create_table_disc(ridepet_synthesis,record_info(fields,ridepet_synthesis),[],set),
	base_db_tools:create_table_disc(ride_proto_db,record_info(fields,ride_proto_db),[],set),
	base_db_tools:create_table_disc(role_level_bonfire_effect_db,record_info(fields,role_level_bonfire_effect_db),[],set),
	base_db_tools:create_table_disc(role_level_experience,record_info(fields,role_level_experience),[],set),
	base_db_tools:create_table_disc(role_level_sitdown_effect_db,record_info(fields,role_level_sitdown_effect_db),[],set),
	base_db_tools:create_table_disc(role_level_soulpower,record_info(fields,role_level_soulpower),[],set),
	base_db_tools:create_table_disc(role_petnum,record_info(fields,role_petnum),[],set),
	base_db_tools:create_table_disc(series_kill,record_info(fields,series_kill),[],set),
	base_db_tools:create_table_disc(skills,record_info(fields,skills),[],bag),
	base_db_tools:create_table_disc(sock, record_info(fields,sock), [], set),
	base_db_tools:create_table_disc(spa_exp,record_info(fields,spa_exp),[],set),
	base_db_tools:create_table_disc(spa_option,record_info(fields,spa_option),[],set),
	base_db_tools:create_table_disc(stonemix, record_info(fields,stonemix), [], set),
	base_db_tools:create_table_disc(system_chat, record_info(fields,system_chat), [], set),
	base_db_tools:create_table_disc(tangle_reward_info,record_info(fields,tangle_reward_info),[],set),
	base_db_tools:create_table_disc(template_itemproto, record_info(fields,template_itemproto), [], set),
	base_db_tools:create_table_disc(timelimit_gift,record_info(fields,timelimit_gift),[],set),
	base_db_tools:create_table_disc(transports,record_info(fields,transports),[],bag),
	base_db_tools:create_table_disc(transport_channel,record_info(fields,transport_channel),[],set),
	base_db_tools:create_table_disc(treasure_chest_drop,record_info(fields,treasure_chest_drop),[],set),
	base_db_tools:create_table_disc(treasure_chest_rate,record_info(fields,treasure_chest_rate),[],set),
	base_db_tools:create_table_disc(treasure_chest_times,record_info(fields,treasure_chest_times),[],set),
	base_db_tools:create_table_disc(treasure_chest_type,record_info(fields,treasure_chest_type),[],set),
	base_db_tools:create_table_disc(treasure_spawns,record_info(fields,treasure_spawns),[],set),
	base_db_tools:create_table_disc(treasure_transport,record_info(fields,treasure_transport),[],bag),
	base_db_tools:create_table_disc(treasure_transport_quality_bonus,record_info(fields,treasure_transport_quality_bonus),[],set),
	base_db_tools:create_table_disc(venation_advanced,record_info(fields,venation_advanced),[],bag),
	base_db_tools:create_table_disc(venation_exp_proto,record_info(fields,venation_exp_proto),[],set),
	base_db_tools:create_table_disc(venation_item_rate,record_info(fields,venation_item_rate),[],set),
	base_db_tools:create_table_disc(venation_point_proto,record_info(fields,venation_point_proto),[],set),
	base_db_tools:create_table_disc(venation_proto,record_info(fields,venation_proto),[],set),
	base_db_tools:create_table_disc(vip_level, record_info(fields,vip_level), [], set),
	base_db_tools:create_table_disc(welfare_activity_data,record_info(fields,welfare_activity_data),[],set),
	base_db_tools:create_table_disc(yhzq_battle,record_info(fields,yhzq_battle),[],set),
	base_db_tools:create_table_disc(yhzq_winner_raward,record_info(fields,yhzq_winner_raward),[],set),
	base_db_tools:create_table_disc(creature_spawns,record_info(fields,creature_spawns),[],set),
	base_db_tools:create_table_disc(template_roleattr,record_info(fields,roleattr),[account,name],set),
	base_db_tools:create_table_disc(template_role_quick_bar,record_info(fields,role_quick_bar),[],set),
	base_db_tools:create_table_disc(template_role_skill,record_info(fields,role_skill),[],set),
	base_db_tools:create_table_disc(template_quest_role,record_info(fields,quest_role),[],set),
	base_db_tools:create_table_disc(instance_quality_proto,record_info(fields,instance_quality_proto),[],set),
	%%宠物技能模板
	base_db_tools:create_table_disc(pet_skill_template, record_info(fields,pet_skill_template), [],bag),
	%%宠物技能槽位
	base_db_tools:create_table_disc(pet_skill_proto, record_info(fields,pet_skill_proto),[], set),
	%%副本元宝委托
	base_db_tools:create_table_disc(instance_entrust,record_info(fields,instance_entrust),[],set),
	base_db_tools:create_table_disc(activity_test01,record_info(fields,activity_test01),[],set),
	%%宠物属性转化率
	base_db_tools:create_table_disc(pet_attr_transform,record_info(fields,pet_attr_transform),[],set),
	%%宠物成长提升
	base_db_tools:create_table_disc(pet_up_growth,record_info(fields,pet_up_growth),[],set),
	%%批量合成概率  by zhangting%%
	base_db_tools:create_table_disc(stonemix_rateinfo,record_info(fields,stonemix_rateinfo),[],set),
	%%宠物技能
	base_db_tools:create_table_disc(pet_skill_book_rate,record_info(fields,pet_skill_book_rate),[],set),
	base_db_tools:create_table_disc(pet_skill_book,record_info(fields,pet_skill_book),[],set),
	base_db_tools:create_table_disc(pet_fresh_skill, record_info(fields,pet_fresh_skill), [], set),
	base_db_tools:create_table_disc(pet_base_attr, record_info(fields,pet_base_attr),[],set),
	%%宠物洗髓
	 base_db_tools:create_table_disc(pet_xisui_rate, record_info(fields,pet_xisui_rate), [], set),
	base_db_tools:create_table_disc(pet_talent_item,record_info(fields,pet_talent_item),[],set),
	base_db_tools:create_table_disc(pet_talent_proto,record_info(fields,pet_talent_proto),[],set),
	base_db_tools:create_table_disc(pet_talent_template, record_info(fields,pet_talent_template),[], bag),
	%%宠物进阶
	base_db_tools:create_table_disc(pet_advance, record_info(fields,pet_advance),[],set),
	base_db_tools:create_table_disc(pet_advance_lucky, record_info(fields,pet_advance),[],bag),
	%%充值礼包
	base_db_tools:create_table_disc(charge_package_proto, record_info(fields,charge_package_proto),[], bag),
	base_db_tools:create_table_disc(item_can_used, record_info(fields,item_can_used),[], bag),
	%%飞剑
	base_db_tools:create_table_disc(wing_level,record_info(fields,wing_level),[],set),
	base_db_tools:create_table_disc(wing_phase,record_info(fields,wing_phase),[],set),
	base_db_tools:create_table_disc(wing_quality,record_info(fields,wing_quality),[],set),
	base_db_tools:create_table_disc(wing_intensify_up,record_info(fields,wing_intensify_up),[],set),
	base_db_tools:create_table_disc(wing_skill,record_info(fields,wing_skill),[],bag),
	base_db_tools:create_table_disc(item_gold_price,record_info(fields,item_gold_price),[],set),
	base_db_tools:create_table_disc(wing_echant,record_info(fields,wing_echant),[],set),
	base_db_tools:create_table_disc(wing_echant_lock,record_info(fields,wing_echant_lock),[],set).

delete_all_tables() ->
	?base_mnesia:delete_table(achieve),
	?base_mnesia:delete_table(activity),
	?base_mnesia:delete_table(activity_value_proto),
	?base_mnesia:delete_table(activity_value_reward),
	?base_mnesia:delete_table(ai_agents),
	?base_mnesia:delete_table(answer),
	?base_mnesia:delete_table(answer_option),
	?base_mnesia:delete_table(attr_info),
	?base_mnesia:delete_table(auto_name),
	?base_mnesia:delete_table(back_echantment_stone),
	?base_mnesia:delete_table(battlefield_proto),
	?base_mnesia:delete_table(block_training),
	?base_mnesia:delete_table(buffers),
	?base_mnesia:delete_table(chat_condition),
	?base_mnesia:delete_table(chess_spirit_config),
	?base_mnesia:delete_table(chess_spirit_rewards),
	?base_mnesia:delete_table(chess_spirit_section),
	?base_mnesia:delete_table(christmas_activity_reward),
	?base_mnesia:delete_table(christmas_tree_config),
	?base_mnesia:delete_table(classbase),
	?base_mnesia:delete_table(congratulations),
	?base_mnesia:delete_table(continuous_logging_gift),
	?base_mnesia:delete_table(country_proto),
	?base_mnesia:delete_table(creature_proto),
	?base_mnesia:delete_table(designation_data),
	?base_mnesia:delete_table(dragon_fight_db),
	?base_mnesia:delete_table(drop_rule),
	?base_mnesia:delete_table(enchantments),
	?base_mnesia:delete_table(enchantments_lucky),
	?base_mnesia:delete_table(enchant_convert),
	?base_mnesia:delete_table(enchant_opt),
	?base_mnesia:delete_table(enchant_property_opt),
	?base_mnesia:delete_table(equipmentset),
	?base_mnesia:delete_table(equipment_fenjie),
	?base_mnesia:delete_table(equipment_move),
	?base_mnesia:delete_table(equipment_sysbrd),
	?base_mnesia:delete_table(equipment_upgrade),
	?base_mnesia:delete_table(everquests),
	?base_mnesia:delete_table(faction_relations),
	?base_mnesia:delete_table(festival_control),
	?base_mnesia:delete_table(festival_recharge_gift),
	?base_mnesia:delete_table(goals),
	?base_mnesia:delete_table(guild_authorities),
	?base_mnesia:delete_table(guild_auth_groups),
	?base_mnesia:delete_table(guild_battle_proto),
	?base_mnesia:delete_table(guild_facilities),
	?base_mnesia:delete_table(guild_monster_proto),
	?base_mnesia:delete_table(guild_setting),
	?base_mnesia:delete_table(guild_shop),
	?base_mnesia:delete_table(guild_shop_items),
	?base_mnesia:delete_table(guild_treasure),
	?base_mnesia:delete_table(guild_treasure_items),
	?base_mnesia:delete_table(guild_treasure_transport_consume),
	?base_mnesia:delete_table(honor_store_items),
	?base_mnesia:delete_table(inlay),
	?base_mnesia:delete_table(instance_proto),
	?base_mnesia:delete_table(item_identify),
	?base_mnesia:delete_table(item_template),
	?base_mnesia:delete_table(jszd_rank_option),
	?base_mnesia:delete_table(levelup_opt),
	?base_mnesia:delete_table(level_activity_rewards_db),
	?base_mnesia:delete_table(loop_instance),
	?base_mnesia:delete_table(loop_instance_proto),
	?base_mnesia:delete_table(loop_tower),
	?base_mnesia:delete_table(lottery_counts),
	?base_mnesia:delete_table(lottery_droplist),
	?base_mnesia:delete_table(mainline_defend_config),
	?base_mnesia:delete_table(mainline_proto),
	?base_mnesia:delete_table(mall_item_info),
	?base_mnesia:delete_table(mall_sales_item_info),
	?base_mnesia:delete_table(map_info),
	?base_mnesia:delete_table(npc_dragon_fight),
	?base_mnesia:delete_table(npc_drop),
	?base_mnesia:delete_table(npc_exchange_list),
	?base_mnesia:delete_table(everquest_list),
	?base_mnesia:delete_table(npc_sell_list),
	?base_mnesia:delete_table(npc_trans_list),
	?base_mnesia:delete_table(quest_npc),
	?base_mnesia:delete_table(npc_functions),
	?base_mnesia:delete_table(offline_everquests_exp),
	?base_mnesia:delete_table(offline_exp),
	?base_mnesia:delete_table(open_service_activities),
	?base_mnesia:delete_table(open_service_activities_time),
	?base_mnesia:delete_table(pet_evolution),
	?base_mnesia:delete_table(pet_explore_gain),
	?base_mnesia:delete_table(pet_explore_style),
	?base_mnesia:delete_table(pet_growth),
	?base_mnesia:delete_table(pet_happiness),
	?base_mnesia:delete_table(pet_level),
	?base_mnesia:delete_table(pet_proto),
	?base_mnesia:delete_table(pet_quality),
	?base_mnesia:delete_table(pet_quality_up),
	?base_mnesia:delete_table(pet_skill_slot),
	?base_mnesia:delete_table(pet_slot),
	?base_mnesia:delete_table(pet_talent_consume),
	?base_mnesia:delete_table(pet_talent_rate),
	?base_mnesia:delete_table(pet_wash_attr_point),
	?base_mnesia:delete_table(pet_item_mall),
	?base_mnesia:delete_table(quests),
	?base_mnesia:delete_table(refine_system),
	?base_mnesia:delete_table(remove_seal),
	?base_mnesia:delete_table(ridepet_synthesis),
	?base_mnesia:delete_table(ride_proto_db),
	?base_mnesia:delete_table(role_level_bonfire_effect_db),
	?base_mnesia:delete_table(role_level_experience),
	?base_mnesia:delete_table(role_level_sitdown_effect_db),
	?base_mnesia:delete_table(role_level_soulpower),
	?base_mnesia:delete_table(role_petnum),
	?base_mnesia:delete_table(series_kill),
	?base_mnesia:delete_table(skills),
	?base_mnesia:delete_table(sock),
	?base_mnesia:delete_table(spa_exp),
	?base_mnesia:delete_table(spa_option),
	?base_mnesia:delete_table(stonemix),
	?base_mnesia:delete_table(system_chat),
	?base_mnesia:delete_table(tangle_reward_info),
	?base_mnesia:delete_table(template_itemproto),
	?base_mnesia:delete_table(timelimit_gift),
	?base_mnesia:delete_table(transports),
	?base_mnesia:delete_table(transport_channel),
	?base_mnesia:delete_table(treasure_chest_drop),
	?base_mnesia:delete_table(treasure_chest_rate),
	?base_mnesia:delete_table(treasure_chest_times),
	?base_mnesia:delete_table(treasure_chest_type),
	?base_mnesia:delete_table(treasure_spawns),
	?base_mnesia:delete_table(treasure_transport),
	?base_mnesia:delete_table(treasure_transport_quality_bonus),
	?base_mnesia:delete_table(venation_advanced),
	?base_mnesia:delete_table(venation_exp_proto),
	?base_mnesia:delete_table(venation_item_rate),
	?base_mnesia:delete_table(venation_point_proto),
	?base_mnesia:delete_table(venation_proto),
	?base_mnesia:delete_table(vip_level),
	?base_mnesia:delete_table(welfare_activity_data),
	?base_mnesia:delete_table(yhzq_battle),
	?base_mnesia:delete_table(yhzq_winner_raward),
	?base_mnesia:delete_table(creature_spawns),
	?base_mnesia:delete_table(template_roleattr),
	?base_mnesia:delete_table(template_role_quick_bar),
	?base_mnesia:delete_table(template_role_skill),
	?base_mnesia:delete_table(template_quest_role),
	?base_mnesia:delete_table(instance_quality_proto),
	%%宠物升级
	?base_mnesia:delete_table(pet_skill_template),
	%%副本元宝委托
	?base_mnesia:delete_table(instance_entrust),
	?base_mnesia:delete_table(activity_test01),
	%%批量合成概率  by zhangting%%
	?base_mnesia:delete_table(stonemix_rateinfo),
	?base_mnesia:delete_table(pet_up_growth),

	?base_mnesia:delete_table(pet_attr_transform),
	%%成就按客户端要求  @@wb20130301
	?base_mnesia:delete_table(achieve_proto),
	?base_mnesia:delete_table(achieve_award),
	?base_mnesia:delete_table(achieve_fuwen),

	?base_mnesia:delete_table(pet_attr_transform),
	?base_mnesia:delete_table(pet_skill_book_rate),
	?base_mnesia:delete_table(pet_skill_book),
	?base_mnesia:delete_table(pet_fresh_skill),
	?base_mnesia:delete_table(pet_xisui_rate),

	?base_mnesia:delete_table(pet_base_attr),
	?base_mnesia:delete_table(pet_talent_item),
	?base_mnesia:delete_table(pet_talent_proto),
	?base_mnesia:delete_table(pet_talent_template),
	?base_mnesia:delete_table(pet_advance),
	?base_mnesia:delete_table(pet_advance_lucky),
	?base_mnesia:delete_table(pet_skill_proto),
	?base_mnesia:delete_table(charge_package_proto),
	?base_mnesia:delete_table(item_can_used),
	%%飞剑
	?base_mnesia:delete_table(wing_level),
	?base_mnesia:delete_table(wing_phase),
	?base_mnesia:delete_table(wing_quality),
	?base_mnesia:delete_table(wing_intensify_up),
	?base_mnesia:delete_table(wing_skill),
	?base_mnesia:delete_table(item_gold_price),
	?base_mnesia:delete_table(wing_echant),
	?base_mnesia:delete_table(wing_echant_lock).

gen_game_db() ->
%%	FileName = "../config/game.config",
%% 	case file:open(FileName,[read]) of 
%% 		{ok,Fd}->
%% 			write_game_db(Fd);
%% 		{error,Reason}-> 
%% 			?base_logger_util:info_msg("Consult error:~p~n",[Reason])
%% 	end.
	FilePath="../config/config",
	base_mnesia_read_config_util:get_object(FilePath).

write_game_db_old_zt(Fd) ->
	case io:read(Fd,'') of
		{error,Reason}->
			?base_logger_util:info_msg("reovery_from failed: ~p~n",[Reason]),
			file:close(Fd);
		eof ->
			file:close(Fd);
		{ok,Term}->
%%			?base_logger_util:info_msg("yanzengyan, Term: ~p~n", [Term]),
			if element(1,Term) =:= continuous_logging_gift ->
					?base_logger_util:info_msg("continuous_logging_op:init_data() 02 Item:~p~n",[Term]),
					if erlang:size(Term)=:=3 -> 
							base_db_dal_util:write( ?base_erlang:append_element(Term,[]));
						true->	  base_db_dal_util:write(Term)
					end;
				true->
					base_db_dal_util:write(Term)
			end,
			write_game_db(Fd),
			ok
	end.

write_game_db(Fd) ->
	case io:read(Fd,'') of
		{error,Reason}->
			?base_logger_util:info_msg("reovery_from failed: ~p~n",[Reason]),
			file:close(Fd);
		eof ->
			file:close(Fd);
		{ok,Term}->
			base_db_dal_util:write(Term),
			write_game_db(Fd),
			ok
	end.


gen_creature_db() ->
	FileName = "../config/creature_spawns.config",
	case file:consult(FileName) of
		{ok,[Terms]}->
			lists:foreach(fun(Term)->add_creature_spawns_to_mnesia(Term)
							end,Terms);
		{error,Reason} ->
			?base_logger_util:info_msg("import_creature_spawns error:~p~n",[Reason])
	end.

add_creature_spawns_to_mnesia(Term)->
	try
		NewTerm = list_to_tuple([creature_spawns|tuple_to_list(Term)]),
		base_db_dal_util:write(NewTerm)
	catch
		E:R-> ?base_logger_util:info_msg("Reason ~p: ~p~n",[E,R]),error
	end.
