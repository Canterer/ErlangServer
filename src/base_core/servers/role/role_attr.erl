-module(role_attr).

-compile(export_all).

-include("data_struct.hrl").
-include("role_struct.hrl").
-include("npc_struct.hrl").
-include("common_define.hrl").
-include("creature_define.hrl").
-include("item_define.hrl").
-include("attr_keyvalue_define.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 一系列属性计算函数
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate_movespeed(MoveSpeedRate,BaseMoveSpeed)->
	erlang:max(1,erlang:trunc(BaseMoveSpeed*(100+ MoveSpeedRate)/100)).
	
calculate_movespeed_by_creature_info(MoveSpeedRate,CreatureInfo)->
	ID = creature_op:get_id_from_creature_info(CreatureInfo),
	case creature_op:what_creature(ID) of
		npc->			
			BaseSpeed = get(run_speed);
		role->
			BaseSpeed = ?BASE_MOVE_SPEED
	end,
	calculate_movespeed(MoveSpeedRate,BaseSpeed).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 属性改变导入玩家信息
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_creature_info({power,Power},RoleInfo) ->
	creature_op:set_power_to_creature_info(RoleInfo,Power);

%%在任何时候,mpmax和hpmax改变的时候,mp和hp都要随着按比率变化


to_creature_info({hp, NewHp},RoleInfo)->
	creature_op:set_life_to_creature_info(RoleInfo,NewHp);

to_creature_info({mp, NewMp},RoleInfo)->
	creature_op:set_mana_to_creature_info(RoleInfo,NewMp);
	
to_creature_info({mpmax, MpMax},RoleInfo) ->
	OldMPMax = creature_op:get_mpmax_from_creature_info(RoleInfo),
	OldMP = creature_op:get_mana_from_creature_info(RoleInfo),
	NewMp = erlang:trunc((MpMax/OldMPMax)*OldMP),
	RolInfo1 = creature_op:set_mpmax_to_creature_info(RoleInfo,MpMax),
	creature_op:set_mana_to_creature_info(RolInfo1,NewMp);

to_creature_info({hpmax, HpMax},RoleInfo) ->
	OldHPMax = creature_op:get_hpmax_from_creature_info(RoleInfo),
	OldHP = creature_op:get_life_from_creature_info(RoleInfo),
	NewHp = erlang:trunc((HpMax/OldHPMax)*OldHP),
	RoleInfo1 = creature_op:set_hpmax_to_creature_info(RoleInfo,HpMax),
	creature_op:set_life_to_creature_info(RoleInfo1,NewHp);

to_creature_info({hprecover, HpRecover},RoleInfo) ->
	if
		is_record(RoleInfo, gm_npc_info)->
			RoleInfo;
		true ->		
			set_hprecover_to_roleinfo(RoleInfo,HpRecover)
	end;

to_creature_info({mprecover, MpRecover},RoleInfo) ->
	if
		is_record(RoleInfo, gm_npc_info)->
			RoleInfo;
		true ->		
			set_mprecover_to_roleinfo(RoleInfo,MpRecover)
	end;

to_creature_info({criticaldestroyrate, CriticalDestoryRate},RoleInfo) ->
	creature_op:set_criticaldamage_to_creature_info(RoleInfo,CriticalDestoryRate);

%%效果里的movespeed影响的是变化率%%直接拿base去算速度,怪物根据跑动速度进行计算
to_creature_info({movespeed, MoveSpeedRate},CreatureInfo) ->
	MoveSpeed = calculate_movespeed_by_creature_info(MoveSpeedRate,CreatureInfo),
	creature_op:set_speed_to_creature_info(CreatureInfo,MoveSpeed);

to_creature_info({magicimmunity, MagicImmue},RoleInfo) ->
	creature_op:set_immunes_to_creature_info(RoleInfo,erlang:setelement(1,creature_op:get_immunes_from_creature_info(RoleInfo),MagicImmue));
to_creature_info({rangeimmunity, RangeImmunity},RoleInfo) ->
	creature_op:set_immunes_to_creature_info(RoleInfo,erlang:setelement(2,creature_op:get_immunes_from_creature_info(RoleInfo),RangeImmunity));
to_creature_info({meleeimmunity, MeleeImmunity},RoleInfo) ->
	creature_op:set_immunes_to_creature_info(RoleInfo,erlang:setelement(3,creature_op:get_immunes_from_creature_info(RoleInfo),MeleeImmunity));

to_creature_info({stamina, Stamina},RoleInfo) ->
	if
		is_record(RoleInfo, gm_npc_info)->
			RoleInfo;
		true ->		
			set_stamina_to_roleinfo(RoleInfo,Stamina)
	end;
	

to_creature_info({strength, Strength},RoleInfo) ->
	if
		is_record(RoleInfo, gm_npc_info)->
			RoleInfo;
		true ->		
			set_strength_to_roleinfo(RoleInfo,Strength)
	end;
	

to_creature_info({intelligence, Intelligence},RoleInfo) ->
	if
		is_record(RoleInfo, gm_npc_info)->
			RoleInfo;
		true ->		
			set_intelligence_to_roleinfo(RoleInfo,Intelligence)
	end;
	

to_creature_info({agile, Aglie},RoleInfo) ->
		if
		is_record(RoleInfo, gm_npc_info)->
			RoleInfo;
		true ->		
			set_agile_to_roleinfo(RoleInfo,Aglie)
	end;

to_creature_info({magicdefense, MagicDefence},RoleInfo) ->
	creature_op:set_defenses_to_creature_info(RoleInfo,erlang:setelement(1,creature_op:get_defenses_from_creature_info(RoleInfo),MagicDefence));
to_creature_info({rangedefense, FarDefence},RoleInfo) ->
	creature_op:set_defenses_to_creature_info(RoleInfo,erlang:setelement(2,creature_op:get_defenses_from_creature_info(RoleInfo),FarDefence));
to_creature_info({meleedefense, NearDefence},RoleInfo) ->
	creature_op:set_defenses_to_creature_info(RoleInfo,erlang:setelement(3,creature_op:get_defenses_from_creature_info(RoleInfo),NearDefence));

to_creature_info({hitrate, Hit},RoleInfo) ->
	creature_op:set_hitrate_to_creature_info(RoleInfo,Hit);

to_creature_info({displayid,NewDisplayId},RoleInfo) ->
	creature_op:set_displayid_to_creature_info(RoleInfo,NewDisplayId);

to_creature_info({dodge, Miss},RoleInfo) ->
	creature_op:set_dodge_to_creature_info(RoleInfo,Miss);

to_creature_info({criticalrate, CriticalDamage},RoleInfo) ->
	creature_op:set_criticalrate_to_creature_info(RoleInfo,CriticalDamage);

to_creature_info({toughness, Toughness},RoleInfo) ->
	creature_op:set_toughness_to_creature_info(RoleInfo,Toughness);

to_creature_info({imprisonment_resist,Imprisonment_resist},RoleInfo) ->
	creature_op:set_debuffimmunes_to_creature_info(RoleInfo,erlang:setelement(1,creature_op:get_debuffimmunes_from_creature_info(RoleInfo),Imprisonment_resist));
to_creature_info({silence_resist,Silence_resist},RoleInfo) ->
	creature_op:set_debuffimmunes_to_creature_info(RoleInfo,erlang:setelement(2,creature_op:get_debuffimmunes_from_creature_info(RoleInfo),Silence_resist));
to_creature_info({daze_resist,Daze_resist},RoleInfo) ->
	creature_op:set_debuffimmunes_to_creature_info(RoleInfo,erlang:setelement(3,creature_op:get_debuffimmunes_from_creature_info(RoleInfo),Daze_resist));
to_creature_info({poison_resist,Poison_resist},RoleInfo) ->
	creature_op:set_debuffimmunes_to_creature_info(RoleInfo,erlang:setelement(4,creature_op:get_debuffimmunes_from_creature_info(RoleInfo),Poison_resist));
to_creature_info({normal_resist,Normal_resist},RoleInfo) ->
	creature_op:set_debuffimmunes_to_creature_info(RoleInfo,erlang:setelement(5,creature_op:get_debuffimmunes_from_creature_info(RoleInfo),Normal_resist));
	
to_creature_info(Attribute,RoleInfo) ->
	base_logger_util:info_msg("!!!!!!!!!!!!!Wrong to_creature_info Attribute:~p RoleInfo ~p~n", [Attribute,RoleInfo]),
	%%throw(error_role_attribute_to_info)
	RoleInfo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 属性转换为相应ID
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%如果当前改变属性里含有hpmax和mpmax,那么hp和mp会随着改变,也需要发送,发送前在此统一检测,并且要将速度变化率改为速度
%%初始化时不要调用此函数,直接设置完善
preform_to_attrs(OriAttrs)->
	ID = creature_op:get_id_from_creature_info(get(creature_info)),
	case lists:keyfind(hpmax,1,OriAttrs) of
		{hpmax,_} ->
			case lists:keyfind(hp,1,OriAttrs) of
				false ->
					Attrs1 = [{hp,creature_op:get_life_from_creature_info(get(creature_info))}]++OriAttrs;
				_->  
					Attrs1  = OriAttrs
			end;
		_ ->
			Attrs1  = OriAttrs
	end,
	case lists:keyfind(mpmax,1,Attrs1) of
		{mpmax,_} ->
			case lists:keyfind(mp,1,Attrs1) of
				false ->
					Attrs2 = [{mp,creature_op:get_mana_from_creature_info(get(creature_info))}]++Attrs1;
				_->  
					Attrs2  = Attrs1
			end;
		_ ->
			Attrs2  = Attrs1
	end,	
	case lists:keyfind(movespeed,1,Attrs1) of
		{movespeed,MoveSpeedRate}->
			MoveSpeed = calculate_movespeed_by_creature_info(MoveSpeedRate,get(creature_info)),			
			lists:keyreplace(movespeed,1,Attrs1,{movespeed,MoveSpeed});
		_->
			Attrs2	
	end.
		
to_role_attribute({class, Race}) ->
	base_pb_util:key_value(?ROLE_ATTR_CLASS, Race);
		
to_role_attribute({level, Level}) ->
	base_pb_util:key_value(?ROLE_ATTR_LEVEL, Level);

to_role_attribute({name,OtherName}) -> 
	base_pb_util:key_value(?ROLE_ATTR_NAME, OtherName);

to_role_attribute({posx,PosX}) ->
	base_pb_util:key_value(?ROLE_ATTR_POSX, PosX);

to_role_attribute({posy,PosY}) ->
	base_pb_util:key_value(?ROLE_ATTR_POSY, PosY);
	
to_role_attribute({gender,Gender}) ->
	base_pb_util:key_value(?ROLE_ATTR_GENDER, Gender);	

to_role_attribute({expr, Expr}) ->
	base_pb_util:key_value(?ROLE_ATTR_EXPR, Expr);

to_role_attribute({hp, HP}) ->
	base_pb_util:key_value(?ROLE_ATTR_HP, HP);

to_role_attribute({mp, MP}) ->
	base_pb_util:key_value(?ROLE_ATTR_MP, MP);

to_role_attribute({mpmax, MpMax}) ->
	base_pb_util:key_value(?ROLE_ATTR_MPMAX, MpMax);
	
to_role_attribute({hpmax, HpMax}) ->
	base_pb_util:key_value(?ROLE_ATTR_HPMAX, HpMax);

to_role_attribute({levelupexpr, Levelupexpr}) ->
	base_pb_util:key_value(?ROLE_ATTR_LEVELUPEXP, Levelupexpr);	

to_role_attribute({boundsilver, Silver}) ->
	base_pb_util:key_value(?ROLE_ATTR_BOUND_SILVER, Silver);

to_role_attribute({silver, Silver}) ->
	base_pb_util:key_value(?ROLE_ATTR_SILVER, Silver);
	%%base_pb_util:key_value(?ROLE_ATTR_SILVER, Silver);
	
to_role_attribute({gold, Gold}) ->
	base_pb_util:key_value(?ROLE_ATTR_GOLD, Gold);

to_role_attribute({ticket, Ticket}) ->
	base_pb_util:key_value(?ROLE_ATTR_TICKET, Ticket);

to_role_attribute({hprecover, HpRecover}) ->
	base_pb_util:key_value(?ROLE_ATTR_HPRECOVER, HpRecover);

to_role_attribute({criticaldestroyrate, CriticalDestoryRate}) ->
	base_pb_util:key_value(?ROLE_ATTR_CRITICALDESTROYRATE, CriticalDestoryRate);

to_role_attribute({mprecover, MpRecover}) ->
	base_pb_util:key_value(?ROLE_ATTR_MPRECOVER, MpRecover);

%%在调用movespeed的转化前,必须preform_to_attrs将变化率转化为实际速度
to_role_attribute({movespeed, MoveSpeed}) ->
	base_pb_util:key_value(?ROLE_ATTR_MOVESPEED, MoveSpeed);

to_role_attribute({fighting_force, Fighting_force}) ->
	base_pb_util:key_value(?ROLE_ATTR_FIGHTING_FORCE, Fighting_force);

to_role_attribute({honor, Honor}) ->
	base_pb_util:key_value(?ROLE_ATTR_HONOR, Honor);

to_role_attribute({meleeimmunity, MeleeImmunity}) ->
	base_pb_util:key_value(?ROLE_ATTR_MELEEIMU, MeleeImmunity);

to_role_attribute({rangeimmunity, RangeImmunity}) ->
	base_pb_util:key_value(?ROLE_ATTR_RANGEIMU, RangeImmunity);

to_role_attribute({magicimmunity, MagicImmue}) ->
	base_pb_util:key_value(?ROLE_ATTR_MAGICIMU, MagicImmue);

to_role_attribute({stamina, Stamina}) ->
	base_pb_util:key_value(?ROLE_ATTR_STAMINA, Stamina);

to_role_attribute({stamina_effect, Stamina}) ->
	base_pb_util:key_value(?ROLE_ATTR_STAMINA, Stamina);

to_role_attribute({strength, Strength}) ->
	base_pb_util:key_value(?ROLE_ATTR_STRENGTH, Strength);

to_role_attribute({strength_effect, Strength}) ->
	base_pb_util:key_value(?ROLE_ATTR_STRENGTH, Strength);

to_role_attribute({intelligence, Intelligence}) ->
	base_pb_util:key_value(?ROLE_ATTR_INTELLIGENCE, Intelligence);

to_role_attribute({intelligence_effect, Intelligence}) ->
	base_pb_util:key_value(?ROLE_ATTR_INTELLIGENCE, Intelligence);

to_role_attribute({agile, Aglie}) ->
	base_pb_util:key_value(?ROLE_ATTR_AGILE, Aglie);

to_role_attribute({agile_effect, Aglie}) ->
	base_pb_util:key_value(?ROLE_ATTR_AGILE, Aglie);

to_role_attribute({magicpower, MagicPower}) ->
	base_pb_util:key_value(?ROLE_ATTR_MAGIC_POWER, MagicPower);
to_role_attribute({meleepower, NearAttack}) ->
	base_pb_util:key_value(?ROLE_ATTR_MELEE_POWER, NearAttack);
to_role_attribute({rangepower, FarAttack}) ->
	base_pb_util:key_value(?ROLE_ATTR_RANGE_POWER, FarAttack);

to_role_attribute({power, Power})->
	base_pb_util:key_value(?ROLE_ATTR_POWER, Power);

to_role_attribute({meleedefense, NearDefence}) ->
	base_pb_util:key_value(?ROLE_ATTR_MELEE_DEFENCE, NearDefence);

to_role_attribute({rangedefense, FarDefence}) ->
	base_pb_util:key_value(?ROLE_ATTR_RANGE_DEFENCE, FarDefence);

to_role_attribute({magicdefense, MagicDefence}) ->
	base_pb_util:key_value(?ROLE_ATTR_MAGIC_DEFENCE, MagicDefence);

to_role_attribute({hitrate, Hit}) ->
	base_pb_util:key_value(?ROLE_ATTR_HITRATE, Hit);

to_role_attribute({dodge, Miss}) ->
	base_pb_util:key_value(?ROLE_ATTR_DODGE, Miss);

to_role_attribute({criticalrate, CriticalDamage}) ->
	base_pb_util:key_value(?ROLE_ATTR_CRITICALRATE, CriticalDamage);

to_role_attribute({toughness, Toughness}) ->
	base_pb_util:key_value(?ROLE_ATTR_TOUGHNESS, Toughness);

to_role_attribute({imprisonment_resist, Value}) ->
	base_pb_util:key_value(?ROLE_ATTR_IMPRISONMENT_RESIST, Value);

to_role_attribute({silence_resist, Value}) ->
	base_pb_util:key_value(?ROLE_ATTR_SILENCE_RESIST, Value);

to_role_attribute({daze_resist, Value}) ->
	base_pb_util:key_value(?ROLE_ATTR_DAZE_RESIST, Value);

to_role_attribute({poison_resist, Value}) ->
	base_pb_util:key_value(?ROLE_ATTR_POISON_RESIST, Value);

to_role_attribute({normal_resist, Value}) ->
	base_pb_util:key_value(?ROLE_ATTR_NORMAL_RESIST, Value);

to_role_attribute({packsize, Toughness}) ->
	base_pb_util:key_value(?ROLE_ATTR_PACKSIZE, Toughness);	

to_role_attribute({storagesize, Toughness}) ->
	base_pb_util:key_value(?ROLE_ATTR_STORAGESIZE, Toughness);
	
to_role_attribute({creature_flag, Npcflag}) ->
	base_pb_util:key_value(?ROLE_ATTR_CREATURE_FLAG, Npcflag);

to_role_attribute({touchred, Value}) ->
	base_pb_util:key_value(?ROLE_ATTR_TOUCHRED, Value);

to_role_attribute({targetid, Value}) ->
	base_pb_util:key_value(?ROLE_ATTR_TARGETID, Value);

to_role_attribute({displayid, Displayid}) ->
	base_pb_util:key_value(?ROLE_ATTR_DISPLAYID, Displayid);

to_role_attribute({templateid, ProtoId}) ->
	base_pb_util:key_value(?ROLE_ATTR_PROTOID, ProtoId);
	
to_role_attribute({buffer, BufferId}) ->
	base_pb_util:key_value(?ROLE_ATTR_BODY_BUFFER, BufferId);

to_role_attribute({buff_level, BufferId}) ->
	base_pb_util:key_value(?ROLE_ATTR_BODY_BUFF_LEVEL, BufferId);
	
to_role_attribute({guildname, Name}) ->
	base_pb_util:key_value(?ROLE_ATTR_GUILD_NAME, Name);
	
to_role_attribute({guildposting, Posting}) ->
	base_pb_util:key_value(?ROLE_ATTR_GUILD_POSTING, Posting);			

to_role_attribute({state, State}) ->
	case is_integer(State) of
		true->
			CreatureState = State;
		_->	
			case State of
				deading->
					CreatureState = ?CREATURE_STATE_DEAD;
				block_training->
					CreatureState = ?CREATURE_STATE_BLOCK_TRAINING;
				sitting->	
					CreatureState = ?CREATURE_STATE_SITDOWN;
				_->
					CreatureState = ?CREATURE_STATE_GAME
			end
	end,		
	base_pb_util:key_value(?ROLE_ATTR_STATE, CreatureState);
	
to_role_attribute({cloth, TemId}) ->
	base_pb_util:key_value(?ROLE_ATTR_LOOKS_CLOTH, TemId);		
	
to_role_attribute({arm, TemId}) ->
	base_pb_util:key_value(?ROLE_ATTR_LOOKS_ARM, TemId);		

to_role_attribute({pkmodel, TemId}) ->
	base_pb_util:key_value(?ROLE_ATTR_PK_MODEL, TemId);
	
to_role_attribute({crime, TemId}) ->
	base_pb_util:key_value(?ROLE_ATTR_CRIME_VALUE, TemId);	

to_role_attribute({view, Enchant}) ->
	base_pb_util:key_value(?ROLE_ATTR_ENCHANT, Enchant);

to_role_attribute({path_x, Pathx}) ->
	base_pb_util:key_value(?ROLE_ATTR_PATH_X, Pathx);	

to_role_attribute({path_y, Pathy}) ->
	base_pb_util:key_value(?ROLE_ATTR_PATH_Y, Pathy);
	
to_role_attribute({ride_display,RideDisplay})->
	base_pb_util:key_value(?ROLE_ATTR_RIDEDISPLAY, RideDisplay);

to_role_attribute({treasure_transport,Treasure_Transport})->
	base_pb_util:key_value(?ROLE_ATTR_TREASURE_TRANSPORT, Treasure_Transport);	
	
to_role_attribute({hpmax_percent,Hpmax_percent})->
	base_pb_util:key_value(?ROLE_ATTR_HPMAX_PERCENT, Hpmax_percent);	
to_role_attribute({meleepower_percent,Meleepower_percent})->
	base_pb_util:key_value(?ROLE_ATTR_MELEEPOWER_PERCENT, Meleepower_percent);	
to_role_attribute({rangepower_percent,Rangepower_percent})->
	base_pb_util:key_value(?ROLE_ATTR_RANGEPOWER_PERCENT, Rangepower_percent);	
to_role_attribute({magicpower_percent,Magicpower_percent})->
	base_pb_util:key_value(?ROLE_ATTR_MAGICPOWER_PERCENT, Magicpower_percent);	
to_role_attribute({movespeed_percent,Movespeed_percent})->
	base_pb_util:key_value(?ROLE_ATTR_MOVESPEED_PERCENT, Movespeed_percent);	
	
%%Pet
to_role_attribute({pet_quality, PetQuality}) ->
	base_pb_util:key_value(?ROLE_ATTR_PET_QUALITY, PetQuality);

to_role_attribute({pet_talents, Talents}) ->
	base_pb_util:key_value(?ROLE_ATTR_PET_TALENTS, Talents);	
	
to_role_attribute({pet_master, Master}) ->
	base_pb_util:key_value(?ROLE_ATTR_PET_MASTER, Master);
	
to_role_attribute({pet_growth,Growth}) ->
	base_pb_util:key_value(?ROLE_ATTR_PET_GROWTH,Growth);

to_role_attribute({pet_stamina,Stamina}) ->
	base_pb_util:key_value(?ROLE_ATTR_PET_STAMINA_ATTR,Stamina);
	
to_role_attribute({pet_slot,Slot})->
	base_pb_util:key_value(?ROLE_ATTR_PET_SLOT,Slot);

to_role_attribute({pet_skill_num,SkillNum})->
	base_pb_util:key_value(?ROLE_ATTR_PET_SKILLNUM,SkillNum);

%to_role_attribute({pet_drop_rate,DropRate})->
	%base_pb_util:key_value(?ROLE_ATTR_PET_DROPRATE,DropRate);

to_role_attribute({pet_power,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_POWER,Value);
to_role_attribute({pet_meleepower,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_POWER,Value);
to_role_attribute({pet_rangepower,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_POWER,Value);
to_role_attribute({pet_magicpower,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_POWER,Value);

to_role_attribute({pet_hitrate,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_HITRATE,Value);

%%宠物资质提升影响属性转换
to_role_attribute({transform,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_TRANSFORM,Value);
%%宠物加速升级key值
to_role_attribute({remain_time,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_RAMINTIME,Value);
to_role_attribute({pet_criticalrate,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_CRITICALRATE,Value);

to_role_attribute({pet_remain_attr,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_REMAINATTR,Value);

to_role_attribute({pet_happiness,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_HAPPINESS,Value);

to_role_attribute({pet_t_power,Value})->
	base_pb_util:key_value(?ROLE_ATTR_T_POWER,Value);

to_role_attribute({pet_t_hitrate,Value})->
	base_pb_util:key_value(?ROLE_ATTR_T_HITRATE,Value);

to_role_attribute({pet_t_criticalrate,Value})->
	base_pb_util:key_value(?ROLE_ATTR_T_CRITICALRATE,Value);

to_role_attribute({pet_t_stamina,Value})->
	base_pb_util:key_value(?ROLE_ATTR_T_STAMINA,Value);

to_role_attribute({pet_quality_value,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_QUALITY_VALUE,Value);

to_role_attribute({pet_quality_up_value,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_QUALITY_UP_VALUE,Value);

to_role_attribute({pet_t_gs,Value})->
	base_pb_util:key_value(?ROLE_ATTR_T_GS,Value);

to_role_attribute({pet_gs_sort,Value})->
	base_pb_util:key_value(?ROLE_ATTR_GS_SORT,Value);

to_role_attribute({pet_lock,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_LOCK,Value);

to_role_attribute({pet_proto,Value})->
	base_pb_util:key_value(?ROLE_ATTR_PET_PROTO,Value);


%%soulpower
to_role_attribute({soulpower,SoulPower})->
	base_pb_util:key_value(?ROLE_ATTR_SOULPOWER,SoulPower);

to_role_attribute({maxsoulpower,MaxSoulPower})->
	base_pb_util:key_value(?ROLE_ATTR_MAXSOULPOWER,MaxSoulPower);
	
to_role_attribute({viptag,VipTag})->
	base_pb_util:key_value(?ROLE_ATTR_VIPTAG,VipTag);

to_role_attribute({serverid,Privilege})->
	base_pb_util:key_value(?ROLE_ATTR_SERVERID,Privilege);

to_role_attribute({faction,Faction})->
	base_pb_util:key_value(?ROLE_ATTR_FACTION,Faction);

to_role_attribute({icon,Icons})->
	base_pb_util:key_value(?ROLE_ATTR_HONOUR,Icons);	
to_role_attribute({picon,Icons})->
	base_pb_util:key_value(?ROLE_ATTR_HONOUR,Icons);	

to_role_attribute({companion_role,RoleId})->
	base_pb_util:key_value(?ROLE_ATTR_COMPANION_ROLE,RoleId);
to_role_attribute({cur_designation,CurDesignationList})->
	base_pb_util:key_value(?ROLE_ATTR_CUR_DESIGNATION,CurDesignationList);
	
to_role_attribute({guildtype,GuildType})->
	base_pb_util:key_value(?ROLE_ATTR_GUILDTYPE,GuildType);
	
to_role_attribute({spiritspower,Value})->
	base_pb_util:key_value(?ROLE_ATTR_SPIRITSPOWER,Value);
	
to_role_attribute({maxspiritspower,Value})->
	base_pb_util:key_value(?ROLE_ATTR_MAXSPIRITSPOWER,Value);
%%枫少添加宠物
to_role_attribute({leveluptime_s,Value})->
	base_pb_util:key_value(?ROLE_ATTR_EXPR,Value);

to_role_attribute({wing_show,Value})->
	case Value of
		0->%%隐藏飞剑
			base_pb_util:key_value(925,Value);
		_->%%乘坐
			case wing_db:get_role_winginfo(get(roleid)) of
				[]->
					base_pb_util:key_value(925,0);
				Info->
					Value1=wing_db:get_wing_phase_from_winginfo(Info),
					base_pb_util:key_value(925,Value1)
			end
	end;

to_role_attribute(Attribute) ->
	io:format("attribute error:~p~p~n", [Attribute,erlang:get_stacktrace()]),
	throw(error_role_attribute).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% item_attr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_item_attribute({enchantments,Ench}) ->
	base_pb_util:key_value(?ITEM_ATTR_ENCH, Ench);

to_item_attribute({count,Count}) ->
	base_pb_util:key_value(?ITEM_ATTR_COUNT, Count);		
	
to_item_attribute({slot,Slot}) ->
	base_pb_util:key_value(?ITEM_ATTR_SLOT, Slot);		
	
to_item_attribute({isbonded,Isbonded}) ->
	base_pb_util:key_value(?ITEM_ATTR_ISBONDED, Isbonded);
 	
to_item_attribute({duration,Duration}) ->
	base_pb_util:key_value(?ITEM_ATTR_DURATION, Duration);

to_item_attribute({ownerid,Ownerid}) ->
	base_pb_util:key_value(?ITEM_ATTR_OWNERID, Ownerid);	
	
to_item_attribute({template_id,Template_id}) ->
	base_pb_util:key_value(?ITEM_ATTR_TEMPLATE_ID, Template_id);
	
to_item_attribute({sockets,Sockets}) ->
	base_pb_util:key_value(?ITEM_ATTR_SOCKETS, Sockets);
	
to_item_attribute({lefttime_s,LeftTime}) ->		
	base_pb_util:key_value(?ITEM_ATTR_LEFTTIME, LeftTime);

	
to_item_attribute({enchant,Enchant}) ->
	lists:map(fun(Term)->
		 			to_role_attribute(Term)
			  end,Enchant);
			  
%%to_item_attribute({socket_2,Template_id}) ->
%%	base_pb_util:key_value(511, Template_id);		

%%to_item_attribute({socket_3,Template_id}) ->
%%	base_pb_util:key_value(512, Template_id);

%%to_item_attribute({socket_4,Template_id}) ->
%%	base_pb_util:key_value(513, Template_id);

%%to_item_attribute({displayid,Displayid}) ->
%%	base_pb_util:key_value(1002, Displayid);
					
to_item_attribute(Attribute) ->
	base_logger_util:info_msg("attribute: error ~p~n", [Attribute]),
	throw(error_item_attribute).
	
to_item_changed_info(LowId,HighId,Attrs,ExtEnchant)->
	base_pb_util:item_changed(LowId,HighId,Attrs,ExtEnchant).
		
to_slot_info({Itemprotoid,Count})->
	base_pb_util:loot_slot_info(Itemprotoid,Count);
	 					
to_slot_info(LootInfo)->
	base_logger_util:info_msg("error LootInfo:~p~n", [LootInfo]).
		
to_teammate_state(RemoteMemberInfo)->
	Roleid = get_id_from_othernode_roleinfo(RemoteMemberInfo),
	LineId = get_lineid_from_othernode_roleinfo(RemoteMemberInfo),
	Level = get_level_from_othernode_roleinfo(RemoteMemberInfo),
	{Posx,Posy} = get_pos_from_othernode_roleinfo(RemoteMemberInfo),
	Life = get_life_from_othernode_roleinfo(RemoteMemberInfo),
	Maxhp = get_hpmax_from_othernode_roleinfo(RemoteMemberInfo),	
	Mana = get_mana_from_othernode_roleinfo(RemoteMemberInfo),
	Maxmp = get_mpmax_from_othernode_roleinfo(RemoteMemberInfo),
	Mapid = get_mapid_from_othernode_roleinfo(RemoteMemberInfo),
	Cloth = get_cloth_from_othernode_roleinfo(RemoteMemberInfo),
	Arm = get_arm_from_othernode_roleinfo(RemoteMemberInfo),
	base_pb_util:to_teammate_state(Roleid, Level,Life, Maxhp, Mana, Maxmp, Posx, Posy, Mapid, LineId,Cloth ,Arm).		

