%% Description: TODO: Add description to role_attr_component
-module(role_attr_component).
-export([
	handle_event/4,
	compute_attrs/2,
	get_role_base_attr/0,
	get_role_other_attr/0,
	recompute_attr/2,
	recompute_base_attr/0
]).

-include("base_component_shared.hrl").
-include("slot_define.hrl").
-include("item_struct.hrl").
-include("role_struct.hrl").

%%计算玩家属性改变，升级和初始化需要全部重新计算

%%初始化计算TODO:current_buffer提前设置
compute_attrs(init,Level) ->
	?ZSS(),
	Class = get(classid),
	%%初始计算	
	% {OriCurrentAttributes,_OriCurrentBuffers,_OriChangeAttribute} = compute_buffers:compute(Class, Level, [], [], [], [],[],[],[],[]),
	{OriCurrentAttributes,_OriCurrentBuffers,_OriChangeAttribute} = apply_component(compute_buffers,compute,[Class, Level, [], [], [], [],[],[],[],[]],{oriCurrentAttributes,oriCurrentBuffers,oriChangeAttribute}),
	put(current_attribute,OriCurrentAttributes),
	%%获取装备属性
	BaseAttr = get_role_base_attr(),
	OtherAttr = get_role_other_attr(),
	put(base_attr,BaseAttr),
	put(other_attr,OtherAttr),
	
	%%属性计算
	{CurrentAttributes,_CurrentBuffers, _ChangeAttribute} 
	= compute_buffers:compute(Class, Level, get(current_attribute), get(current_buffer), [], [],BaseAttr,[],OtherAttr,[]),
	put(current_attribute, CurrentAttributes),
	Power = attribute:get_current(CurrentAttributes,power),
	Hprecover = attribute:get_current(CurrentAttributes,hprecover),
	CriDerate = attribute:get_current(CurrentAttributes,criticaldestroyrate),
	Mprecover = attribute:get_current(CurrentAttributes,mprecover),
	MovespeedRate = attribute:get_current(CurrentAttributes,movespeed),
	Meleeimmunity = attribute:get_current(CurrentAttributes,meleeimmunity),
	Rangeimmunity = attribute:get_current(CurrentAttributes,rangeimmunity),
	Magicimmunity = attribute:get_current(CurrentAttributes,magicimmunity),
	Hpmax = attribute:get_current(CurrentAttributes,hpmax),
	Mpmax = attribute:get_current(CurrentAttributes,mpmax),
	Stamina = attribute:get_current(CurrentAttributes,stamina),
	Strength = attribute:get_current(CurrentAttributes,strength),
	Intelligence = attribute:get_current(CurrentAttributes,intelligence),
	Agile = attribute:get_current(CurrentAttributes,agile),
	Meleedefense = attribute:get_current(CurrentAttributes,meleedefense),
	Rangedefense = attribute:get_current(CurrentAttributes,rangedefense),
	Magicdefense = attribute:get_current(CurrentAttributes,magicdefense),
	Hitrate = attribute:get_current(CurrentAttributes,hitrate),
	Dodge = attribute:get_current(CurrentAttributes,dodge),
	Criticalrate = attribute:get_current(CurrentAttributes,criticalrate),
	Toughness = attribute:get_current(CurrentAttributes,toughness),
	Imprisonment_resist = attribute:get_current(CurrentAttributes,imprisonment_resist),
	Silence_resist = attribute:get_current(CurrentAttributes,silence_resist),
	Daze_resist = attribute:get_current(CurrentAttributes,daze_resist),
	Poison_resist = attribute:get_current(CurrentAttributes,poison_resist),
	Normal_resist = attribute:get_current(CurrentAttributes,normal_resist),
	Fighting_force = role_fighting_force:computter_fight_force(Hpmax,Power,Meleedefense,Rangedefense,Magicdefense,Hitrate,Dodge,Criticalrate,
					  CriDerate,Toughness,Meleeimmunity,Rangeimmunity,Magicimmunity),
	{Power,Hprecover,CriDerate,Mprecover,MovespeedRate,Meleeimmunity,Rangeimmunity,Magicimmunity,Hpmax,Mpmax,Stamina,Strength,
	Intelligence,Agile,Meleedefense,Rangedefense,Magicdefense,Hitrate,Dodge,Criticalrate,Toughness,Imprisonment_resist,Silence_resist,
	Daze_resist,Poison_resist,Normal_resist,Fighting_force}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%										物品的使用和装备
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%%获取人物的基础属性 包含装备和修为
%%
get_role_base_attr()->
	%venation_op:get_venation_attr() ++ get_all_bodyitems_attr() ++ get_skill_add_attr()++wing_op:wing_to_attr_role().
	venation_op:get_venation_attr() ++ get_all_bodyitems_attr() ++ get_skill_add_attr().
	
%%
%%获取人物的其他属性,WB20130417 add achieve
%%
get_role_other_attr()->
	designation_op:get_designation_attr() ++ get_pet_add_attr()++achieve_op:get_achieve_add_attr()++furnace_op:get_furnace_add_attribute()++astrology_op:get_astrology_add_attribute().
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%得到目前身上所有物品属性，计算套装和全星触发
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%						
get_all_bodyitems_attr()->
	BodyItemsId = package_op:get_body_items_id(),
	BodyItemsInfo = lists:map(fun(Id)-> items_op:get_item_info(Id) end,BodyItemsId ),
	ItemsAttr1 = lists:foldl(fun(ItemInfo,Attr)->
			case ItemInfo =/= [] of
				true ->
					items_op:get_item_attr(ItemInfo)++Attr ;
				false ->	
					base_logger_util:info_msg("get_all_bodyitems_attr,error Itemid in ~p ~n",[BodyItemsId]),
					Attr
			end
			end,[],BodyItemsInfo),
	EnchantmensetsItems = lists:filter(fun(ItemInfoTmp)->
					Slot = get_slot_from_iteminfo(ItemInfoTmp),
					(Slot =/= ?MANTEAU_SLOT) and (Slot =/= ?FASHION_SLOT) and (Slot =/= ?RIDE_SLOT)
				end,BodyItemsInfo),
	case erlang:length(EnchantmensetsItems) >= ?SLOT_BODY_ENDEX -3 of
		true ->				%% 原始装备属性+全星属性+套装属性 
			MinEnchant = get_item_enchantmentset(BodyItemsInfo),
			apply_enchantments_changed(MinEnchant),
			ItemsAttr2 = get_item_enchantmentset_attr(MinEnchant) ++  get_equip_set_attr(BodyItemsInfo)++ItemsAttr1;							
		false ->			%%装备不足，只计算套装属性
			apply_enchantments_changed(0),
			ItemsAttr2 =  get_equip_set_attr(BodyItemsInfo) ++ ItemsAttr1
	end,
	%% 当前宠物添加的属性
%%	ItemsAttr3 = get_pet_add_attr() ++ get_skill_add_attr() ++ ItemsAttr2,
	%% 经脉添加的属性
%%	ItemsAttr4 = venation_op:get_venation_attr() ++ ItemsAttr3,
%%	ItemsAttr5 = designation_op:get_designation_attr()++ItemsAttr4,
%%	ItemsAttr5.
	ItemsAttr2.

%%得到当前宠物附加给角色的属性
get_pet_add_attr()->
	pet_op:get_pet_add_attr().

%%被动技能附加属性	
get_skill_add_attr()->
	SkillBuff = skill_op:get_skill_add_attr(),
	SkillBuff.	


get_item_enchantmentset(BodyItemsInfo)->
	MinEnchant = lists:foldl(fun(Info,MinEnt)->
		Ent = get_enchantments_from_iteminfo(Info),
		Level = get_level_from_iteminfo(Info),
		Slot = get_slot_from_iteminfo(Info),
		case (Slot =:= ?MANTEAU_SLOT) or (Slot =:= ?FASHION_SLOT) or (Slot =:= ?RIDE_SLOT) of
			true ->
				MinEnt;
			false ->	 
				if 
					(Ent < MinEnt) -> 
						Ent;
					true -> 
						MinEnt
				end
		end
	end,?MAX_ENCHANTMENTS+1,BodyItemsInfo),
	if
		MinEnchant=:= ?MAX_ENCHANTMENTS+1->
			0;
		true->	
			MinEnchant
	end.

get_item_enchantmentset_attr(MinEnchant)->
	if 
		(MinEnchant=:=0)->
			[];
		true ->
			case enchantments_db:get_enchantments_info(MinEnchant) of
				[]->
					[];
				EnchantmentInfo->
					enchantments_db:get_enchantments_set_attr(EnchantmentInfo)
			end
	end.

apply_enchantments_changed(Enchant)->
	case get_view_from_roleinfo(get(creature_info)) of
		Enchant->
			nothing;
		_->
			put(creature_info,set_view_to_roleinfo(get(creature_info),Enchant)),
			% self_update_and_broad([{view,Enchant}])
			apply_component(role_update_broad_component,self_update_and_broad,[[{view,Enchant}]])
	end.

%%套装属性
get_equip_set_attr(BodyItemsInfo)->
	ItemSets = lists:foldl(fun(ItemInfo,SetsTmp)->
								SetId = get_equipmentset_from_iteminfo(ItemInfo),
								ItemTmpId = get_template_id_from_iteminfo(ItemInfo),
								if
									SetId =/= 0 -> 
											case lists:keyfind(SetId,1,SetsTmp) of
												false ->
													Sets = [{SetId,[ItemTmpId]}]++SetsTmp;
												{SetId,ItemTmpIds} ->
													ItemSetTempIds = equipmentset_db:get_equipmentset_includes(SetId),
													lists:foldl(fun([NotBound,Bound],Acc)->
																	case lists:member(ItemTmpId,[NotBound,Bound]) of
																		true->
																			[LeftId] = lists:delete([NotBound,Bound],[ItemTmpId]),
																			case lists:member(LeftId,ItemTmpIds) of
																				true->
																					Acc;
																				_->
																					lists:keyreplace(SetId,1,SetsTmp,{SetId,[ItemTmpId|ItemTmpIds]})
																			end;
																		_->
																			Acc
																	end
																end,SetsTmp,ItemSetTempIds),
													Sets  = lists:keyreplace(SetId,1,SetsTmp,{SetId,[ItemTmpId|ItemTmpIds]})
											end,
											Sets;
									true ->
											SetsTmp								
								end end,[],BodyItemsInfo),								
	ItemsSetAttr = lists:foldl( fun ({SetId,ItemIdTmps},Attrs)->
									ItemSetTmpIds = equipmentset_db:get_equipmentset_includes(SetId),
									Count = length(ItemIdTmps),
									Attr = equipmentset_db:get_equipmentset_states(SetId,Count),
									Attr ++ Attrs
								end,[],ItemSets),
	ItemsSetAttr.

%%重新计算buff
recompute_attr(NewBuffers2,RemoveBuff)->
	BaseAttr = get(base_attr),
	RemoveBaseAttr = [],
	OtherAttr = get(other_attr),
	RemoveAotherAttr = [], 
	recompute_attr(NewBuffers2,RemoveBuff,BaseAttr,RemoveBaseAttr,OtherAttr,RemoveAotherAttr).

%%重新计算基础属性（装备，修为，技能 ）	
recompute_base_attr()->
	RemoveBaseAttr = get(base_attr),
	NewBaseAttr = lists:filter(fun({_,Value})->Value=/=0 end,get_role_base_attr()),
	put(base_attr,NewBaseAttr),
	OtherAttr = get(other_attr),
	RemoveOtherAttr = [], 
	recompute_attr([], [],NewBaseAttr,RemoveBaseAttr,OtherAttr,RemoveOtherAttr).
	
%%重新计算其他属性（宠物等）	
recompute_other_attr()->
	RemoveOtherAttr = get(other_attr),
	OtherAttr = lists:filter(fun({_,Value})->Value=/=0 end,get_role_other_attr()),
	put(other_attr,OtherAttr),
	BaseAttr = get(base_attr),
	RemoveBaseAttr = [], 
	recompute_attr([], [],BaseAttr,RemoveBaseAttr,OtherAttr,RemoveOtherAttr).
	

%%
%%重新计算装备的属性
%%
recompute_equipment_attr()->
	recompute_base_attr().
	
recompute_venation_attr()->
	recompute_base_attr().	

recompute_designation_attr()->
	recompute_other_attr().
	
recompute_skill_attr()->
	recompute_base_attr().
	
recompute_pet_attr()->
	recompute_other_attr().
	
		
%%重新计算所有属性和buff	新增buff,删除buff,调用在删除或新增buff之后又需要计算装备更换时,省去一次recompute_attr
recompute_all_attr_after_buff(NewBuffers,RemoveBuff)->
	RemoveBaseAttr = get(base_attr),
	NewBaseAttr = lists:filter(fun({_,Value})->Value=/=0 end,get_role_base_attr()),
	put(base_attr,NewBaseAttr),
	RemoveOtherAttr = get(other_attr),
	OtherAttr = lists:filter(fun({_,Value})->Value=/=0 end,get_role_other_attr()),		 
	put(other_attr,OtherAttr),
	recompute_attr(NewBuffers,RemoveBuff,NewBaseAttr,RemoveBaseAttr,OtherAttr,RemoveOtherAttr).		

recompute_attr(NewBuffers2,RemoveBuff,BaseAttr,RemoveAttr,OtherAttr,RemoveOtherAttr)->
	SelfId = get(roleid),
	{NewAttributes, _CurrentBuffers, ChangeAttribute} = 
	compute_buffers:compute(get(classid), get(level), get(current_attribute), get(current_buffer), NewBuffers2, RemoveBuff,BaseAttr,RemoveAttr,OtherAttr,RemoveOtherAttr),
	%%应用属性改变
	put(current_attribute, NewAttributes),
	OriInfo = get(creature_info),
	NewInfo = lists:foldl(fun(Attr,Info)->					
				 	role_attr:to_creature_info(Attr,Info)
				 end,OriInfo,ChangeAttribute),	
	put(creature_info,NewInfo),
	update_role_info(SelfId,get(creature_info)),
	%%发送属性改变
	ChangeAttribute_Hp_Mp = role_attr:preform_to_attrs(ChangeAttribute),
	% self_update_and_broad(ChangeAttribute_Hp_Mp).
	apply_component(role_update_broad_component,self_update_and_broad,[ChangeAttribute_Hp_Mp]).

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.

update_role_info()->
	update_role_info(get(roleid),get(creature_info)).	
update_role_info(RoleId, RoleInfo) ->
	base_role_manager:regist_role_info(RoleId, RoleInfo).