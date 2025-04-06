%% Description: TODO: Add description to role_money_component
-module(role_money_component).
-export([
	handle_event/4,
	check_money_old/2,
	account_charge/2
]).

-include("base_component_shared.hrl").
-include("common_define.hrl").
-include("mnesia_table_def.hrl").
-include("role_struct.hrl").

%%
%%return true|false
%%	
check_money_old(MoneyType, MoneyCount)->
	case MoneyType of
		?MONEY_BOUND_SILVER ->
			BoundSilver = get_boundsilver_from_roleinfo(get(creature_info)),
			Silver = get_silver_from_roleinfo(get(creature_info)),
			(BoundSilver + Silver) >= abs(MoneyCount);
		?MONEY_SILVER ->
			get_silver_from_roleinfo(get(creature_info)) >= abs(MoneyCount);
		?MONEY_GOLD ->
			AccountName = get(account_id),
			case dal:read_rpc(account, AccountName) of
				{ok,[Account]}->
					#account{username=User,roleids=RoleIds,gold=OGold} = Account,
					if 
						OGold >= abs(MoneyCount) ->
							true;
						true->
							false
					end;
				_->
					base_logger_util:info_msg(" check_money error !!! ~p ~n",[AccountName]),
					false		
			end;			
		?MONEY_TICKET->
			get_ticket_from_roleinfo(get(creature_info))>= abs(MoneyCount);
		?MONEY_CHARGE_INTEGRAL->
			{Charge_integral,_} = get(role_mall_integral),
			Charge_integral >= abs(MoneyCount);
		?MONEY_CONSUMPTION_INTEGRAL->
			{_,Consume_integral} = get(role_mall_integral),
			Consume_integral >= abs(MoneyCount);
		?MONEY_HONOR-> 
			Honor = get_honor_from_roleinfo(get(creature_info)) >= abs(MoneyCount);
		_->
			false
	end.

account_charge(IncGold,NewGold)->
	put(creature_info,set_gold_to_roleinfo(get(creature_info), NewGold)),
	NewAttrGold =[{gold, NewGold}],
	% only_self_update(NewAttrGold).
	apply_component(role_update_broad_component,only_self_update,[NewAttrGold]).
%% 	achieve_op:achieve_update({money},[?MONEY_GOLD],NewGold).

%%zhangting 获取当前账户
get_curr_account()->
	 AccountName = get(account_id),
	 case dal:read_rpc(account, AccountName) of
				{ok,[Account]}->
					Account;
				_->
					base_logger_util:info_msg(" get_curr_account error !!! ~p ~n",[AccountName]),
					[]		
	 end.			

check_money(MoneyType, MoneyCount)->
	case MoneyType of
		?MONEY_BOUND_SILVER ->
			BoundSilver = get_boundsilver_from_roleinfo(get(creature_info)),
			Silver = get_silver_from_roleinfo(get(creature_info)),
			(BoundSilver + Silver) >= abs(MoneyCount);
		?MONEY_SILVER ->
			get_silver_from_roleinfo(get(creature_info)) >= abs(MoneyCount);
		?MONEY_GOLD ->
			AccountName = get(account_id),
			case dal:read_rpc(account, AccountName) of
				{ok,[Account]}->
					#account{username=User,roleids=RoleIds,gold=OGold,qq_gold=QQ_gold,local_gold=Local_gold} = Account,
				   case base_env_ets:get(use_qq_pay_flag, 0) of
				     0-> 
					     if Local_gold >= abs(MoneyCount) ->true;true->false end;
					 1-> 
					     if QQ_gold >= abs(MoneyCount) ->true;true->false end;   
				     2->	  
					     if OGold >= abs(MoneyCount) ->true;true->false end
				   end;
					
				_->
					base_logger_util:info_msg(" check_money error !!! ~p ~n",[AccountName]),
					false		
			end;			
		?MONEY_TICKET->
			get_ticket_from_roleinfo(get(creature_info))>= abs(MoneyCount);
		?MONEY_CHARGE_INTEGRAL->
			{Charge_integral,_} = get(role_mall_integral),
			Charge_integral >= abs(MoneyCount);
		?MONEY_CONSUMPTION_INTEGRAL->
			{_,Consume_integral} = get(role_mall_integral),
			Consume_integral >= abs(MoneyCount);
		?MONEY_HONOR-> 
			Honor = get_honor_from_roleinfo(get(creature_info)) >= abs(MoneyCount);
		_->
			false
	end.


money_change(MoneyType, MoneyCount,Reason) ->
  if 	(MoneyType =:= ?MONEY_GOLD ) and (MoneyCount =/= 0) ->
	   if MoneyCount>0 ->
       	  case base_env_ets:get(use_qq_pay_flag, 0) of
			     1-> 
				    nothing;
			     _->	
				     #account{username=User, gold=OGold,qq_gold=QQ_gold,local_gold=Local_gold} = get_curr_account(),
                   money_change_gold(MoneyType, MoneyCount,Reason,0,MoneyCount+Local_gold)
			    end;
		true->
          online_gold_change(MoneyType, MoneyCount,Reason)
		end;
  true->
      money_change_not_gold(MoneyType, MoneyCount,Reason)
  end.


online_gold_change(MoneyType, MoneyCount,Reason) ->
	ReasonList=
		case erlang:is_atom(Reason)		of
			true->erlang:atom_to_list(Reason);
			_->Reason
		end,
													  
    #account{username=User, gold=OGold,qq_gold=QQ_gold,local_gold=Local_gold} = get_curr_account(),
	Amt=abs(MoneyCount),
	 case base_env_ets:get(use_qq_pay_flag, 0) of
	  0->	 money_change_gold(MoneyType, MoneyCount,Reason,0,Local_gold-Amt);
	  1-> 
          {ok,Balance}= qq_gold_op:do_qq_pay(Amt,ReasonList),
          money_change_gold(MoneyType, MoneyCount,Reason,Balance,0);
	  2->	  
       
		  if QQ_gold> Amt ->
		     {ok,Balance}=  qq_gold_op:do_qq_pay(Amt,ReasonList),
			  money_change_gold(MoneyType, MoneyCount,Reason,Balance,Local_gold);
		  true->
			  if QQ_gold>0 ->
                {ok,Balance} = qq_gold_op:do_qq_pay(QQ_gold,ReasonList), 
                  money_change_gold(MoneyType, MoneyCount,Reason,Balance,Local_gold - (Amt-QQ_gold));
             true->
  				  money_change_gold(MoneyType, MoneyCount,Reason,0,Local_gold-Amt)
			  end
         end
	  end.


money_change_gold(MoneyType, MoneyCount,Reason,QQ_gold,Local_gold) ->
	RoleInfo = get(creature_info),
	AccountName = get(account_id),
	Roleid = get(roleid),
	Transaction = 
	fun()->
		case mnesia:read(account, AccountName) of
			[]->
				[];
			[Account]->
				#account{username=User, gold=OGold} = Account,
				NewGold = OGold+MoneyCount,
				NewAccount = Account#account{gold=NewGold,qq_gold=QQ_gold,local_gold=Local_gold},
				mnesia:write(NewAccount),
				NewAccount
		end
	end,
	case dal:run_transaction_rpc(Transaction) of
		{failed,badrpc,_Reason}->
			base_logger_util:info_msg("money_change gold badrpc error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount]);
		{faild,Reason}->
			base_logger_util:info_msg("money_change gold faild error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount]);
		{ok,[]}->
			base_logger_util:info_msg("money_change gold Account error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount]);
		{ok,Result}->
			#account{username=User,roleids=RoleIds,gold=ReGold} = Result,
			NewRoleInfo = set_gold_to_roleinfo(RoleInfo, ReGold),
			NewAttrGold =[{gold, ReGold}],
			% only_self_update(NewAttrGold),
			apply_component(role_update_broad_component,only_self_update,[NewAttrGold]),
			put(creature_info, NewRoleInfo),
%% 			achieve_op:achieve_update({money},[?MONEY_GOLD],ReGold),	
			gold_exchange:consume_gold_change(-MoneyCount,Reason),
			consume_return:gold_change(-MoneyCount,Reason),
			gm_logger_role:role_gold_change(User,Roleid,MoneyCount,ReGold,Reason),
			FRole = fun(RoleId) ->
				case role_pos_util:where_is_role(RoleId) of
					[]->
						nothing;
					RolePos->
						if
							RoleId =/= Roleid->
								Node = role_pos_db:get_role_mapnode(RolePos),
								Proc = role_pos_db:get_role_pid(RolePos),
								role_processor:account_charge(Node, Proc, {account_charge,MoneyCount,ReGold});
							true->
								nothing
						end
				end
			end,
			lists:foreach(FRole, RoleIds);
		_->
			base_logger_util:info_msg("money_change gold unknow error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount])
	end.


  
money_change_not_gold(MoneyType, MoneyCount,Reason) ->
	RoleInfo = get(creature_info),
	if
		(MoneyType =:=?MONEY_BOUND_SILVER) and (MoneyCount=/= 0) ->
			%% 银币	
			HasBoundSilver = get_boundsilver_from_roleinfo(RoleInfo),
			case HasBoundSilver + MoneyCount < 0 of
				true->		
					NewSilver = 0,
					money_change(?MONEY_SILVER,HasBoundSilver + MoneyCount,Reason);
				_->
					NewSilver = HasBoundSilver + MoneyCount
			end,
			NewRoleInfo = set_boundsilver_to_roleinfo(get(creature_info), NewSilver),
			NewAttrSilver = [{boundsilver, NewSilver}],
			% only_self_update(NewAttrSilver),
			apply_component(role_update_broad_component,only_self_update,[NewAttrSilver]),
			gm_logger_role:role_boundsilver_change(get(roleid),MoneyCount,NewSilver,Reason,get(level)),
			put(creature_info, NewRoleInfo);
%% 			achieve_op:achieve_update({money},[?MONEY_BOUND_SILVER],NewSilver);	
		(MoneyType =:= ?MONEY_SILVER) and (MoneyCount=/= 0) ->
			NewSilver = get_silver_from_roleinfo(RoleInfo)+ MoneyCount,	
			NewRoleInfo = set_silver_to_roleinfo(RoleInfo, NewSilver),
			NewAttrSilver = [{silver, NewSilver}],	
			% only_self_update(NewAttrSilver),
			apply_component(role_update_broad_component,only_self_update,[NewAttrSilver]),
			gm_logger_role:role_silver_change(get(roleid),MoneyCount,NewSilver,Reason,get(level)),
			put(creature_info, NewRoleInfo);
		(MoneyType =:= ?MONEY_TICKET) and (MoneyCount=/= 0)->
			%% 礼券
			NewTick = get_ticket_from_roleinfo(RoleInfo) + MoneyCount,
			NewRoleInfo = set_ticket_to_roleinfo(RoleInfo, NewTick),
			NewAttrTick = [{ticket, NewTick}],
			% only_self_update(NewAttrTick),
			apply_component(role_update_broad_component,only_self_update,[NewAttrTick]),
			gm_logger_role:role_ticket_change(get(roleid),MoneyCount,NewTick,Reason,get(level)),
			put(creature_info, NewRoleInfo);
%% 			achieve_op:achieve_update({money},[?MONEY_TICKET],NewTick);
		(MoneyType =:= ?MONEY_CHARGE_INTEGRAL) and (MoneyCount=/= 0)->
			{Charge_integral,Consume_integral} = get(role_mall_integral),
			NewCharge_Integral = Charge_integral + MoneyCount,
			put(role_mall_integral,{NewCharge_Integral,Consume_integral}),
			mall_integral_db:add_role_mall_integral(get(roleid),NewCharge_Integral,Consume_integral),
			Message = login_pb:encode_change_role_mall_integral_s2c(#change_role_mall_integral_s2c{charge_integral=NewCharge_Integral,by_item_integral=Consume_integral}),
			% send_data_to_gate(Message),
			apply_component(send_to_gate_component,send_data_to_gate,[Message]),
			gm_logger_role:role_charge_integral_change(get(roleid),MoneyCount,NewCharge_Integral,Reason,get(level));
		(MoneyType =:= ?MONEY_CONSUMPTION_INTEGRAL) and (MoneyCount=/= 0)->
			{Charge_integral,Consume_integral} = get(role_mall_integral),
			NewConsum_Integral = Consume_integral + MoneyCount,
			put(role_mall_integral,{Charge_integral,NewConsum_Integral}),
			mall_integral_db:add_role_mall_integral(get(roleid),Charge_integral,NewConsum_Integral),
			Message = login_pb:encode_change_role_mall_integral_s2c(#change_role_mall_integral_s2c{charge_integral=Charge_integral,by_item_integral=NewConsum_Integral}),
			% send_data_to_gate(Message),
			apply_component(send_to_gate_component,send_data_to_gate,[Message]),
			gm_logger_role:role_consume_integral_change(get(roleid),MoneyCount,NewConsum_Integral,Reason,get(level));
		(MoneyType =:= ?MONEY_HONOR) and (MoneyCount=/= 0)->
			NewHonor = get_honor_from_roleinfo(RoleInfo) + MoneyCount,
			NewRoleInfo = set_honor_to_roleinfo(RoleInfo, NewHonor),
			NewAttrHonor = [{honor, NewHonor}],
			% only_self_update(NewAttrHonor),
			apply_component(role_update_broad_component,only_self_update,[NewAttrHonor]),
			put(creature_info, NewRoleInfo),
			gm_logger_role:role_honor_change(get(roleid),MoneyCount,NewHonor,Reason,get(level));
		(MoneyType =:= ?TYPE_GUILD_CONTRIBUTION) and (MoneyCount=/= 0)->
			case guild_util:is_have_guild() of
				true->
					guild_op:contribute(MoneyCount);
				_->
					ignor
			end;
		true->
			nothing
	end.


money_change_old(MoneyType, MoneyCount,Reason) ->
	RoleInfo = get(creature_info),
	if
		(MoneyType =:=?MONEY_BOUND_SILVER) and (MoneyCount=/= 0) ->
			%% 银币	
			HasBoundSilver = get_boundsilver_from_roleinfo(RoleInfo),
			case HasBoundSilver + MoneyCount < 0 of
				true->		
					NewSilver = 0,
					money_change(?MONEY_SILVER,HasBoundSilver + MoneyCount,Reason);
				_->
					NewSilver = HasBoundSilver + MoneyCount
			end,
			NewRoleInfo = set_boundsilver_to_roleinfo(get(creature_info), NewSilver),
			NewAttrSilver = [{boundsilver, NewSilver}],
			% only_self_update(NewAttrSilver),
			apply_component(role_update_broad_component,only_self_update,[NewAttrSilver]),
			gm_logger_role:role_boundsilver_change(get(roleid),MoneyCount,NewSilver,Reason,get(level)),
			put(creature_info, NewRoleInfo);
%% 			achieve_op:achieve_update({money},[?MONEY_BOUND_SILVER],NewSilver);	
		(MoneyType =:= ?MONEY_SILVER) and (MoneyCount=/= 0) ->
			NewSilver = get_silver_from_roleinfo(RoleInfo)+ MoneyCount,	
			NewRoleInfo = set_silver_to_roleinfo(RoleInfo, NewSilver),
			NewAttrSilver = [{silver, NewSilver}],	
			% only_self_update(NewAttrSilver),
			apply_component(role_update_broad_component,only_self_update,[NewAttrSilver]),
			gm_logger_role:role_silver_change(get(roleid),MoneyCount,NewSilver,Reason,get(level)),
			put(creature_info, NewRoleInfo);
		(MoneyType =:= ?MONEY_GOLD ) and (MoneyCount =/= 0)->
			AccountName = get(account_id),
			Roleid = get(roleid),
			Transaction = 
			fun()->
				case mnesia:read(account, AccountName) of
					[]->
						[];
					[Account]->
						#account{username=User, gold=OGold} = Account,
						NewGold = OGold+MoneyCount,
						NewAccount = Account#account{gold=NewGold},
						mnesia:write(NewAccount),
						NewAccount
				end
			end,



			case dal:run_transaction_rpc(Transaction) of
				{failed,badrpc,_Reason}->
					base_logger_util:info_msg("money_change gold badrpc error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount]);
				{faild,Reason}->
					base_logger_util:info_msg("money_change gold faild error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount]);
				{ok,[]}->
					base_logger_util:info_msg("money_change gold Account error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount]);
				{ok,Result}->
					#account{username=User,roleids=RoleIds,gold=ReGold} = Result,
					NewRoleInfo = set_gold_to_roleinfo(RoleInfo, ReGold),
					NewAttrGold =[{gold, ReGold}],
					% only_self_update(NewAttrGold),
					apply_component(role_update_broad_component,only_self_update,[NewAttrGold]),
					put(creature_info, NewRoleInfo),
%% 					achieve_op:achieve_update({money},[?MONEY_GOLD],ReGold),	
					gold_exchange:consume_gold_change(-MoneyCount,Reason),
					consume_return:gold_change(-MoneyCount,Reason),
					gm_logger_role:role_gold_change(User,Roleid,MoneyCount,ReGold,Reason),
					FRole = fun(RoleId) ->
						case role_pos_util:where_is_role(RoleId) of
							[]->
								nothing;
							RolePos->
								if
									RoleId =/= Roleid->
										Node = role_pos_db:get_role_mapnode(RolePos),
										Proc = role_pos_db:get_role_pid(RolePos),
										role_processor:account_charge(Node, Proc, {account_charge,MoneyCount,ReGold});
									true->
										nothing
								end
						end
					end,
					lists:foreach(FRole, RoleIds);
				_->
					base_logger_util:info_msg("money_change gold unknow error!!!! (account,~p) error! Gold ~p ",[AccountName,MoneyCount])
			end;	
		(MoneyType =:= ?MONEY_TICKET) and (MoneyCount=/= 0)->
			%% 礼券
			NewTick = get_ticket_from_roleinfo(RoleInfo) + MoneyCount,
			NewRoleInfo = set_ticket_to_roleinfo(RoleInfo, NewTick),
			NewAttrTick = [{ticket, NewTick}],
			% only_self_update(NewAttrTick),
			apply_component(role_update_broad_component,only_self_update,[NewAttrTick]),
			gm_logger_role:role_ticket_change(get(roleid),MoneyCount,NewTick,Reason,get(level)),
			put(creature_info, NewRoleInfo);
%% 			achieve_op:achieve_update({money},[?MONEY_TICKET],NewTick);
		(MoneyType =:= ?MONEY_CHARGE_INTEGRAL) and (MoneyCount=/= 0)->
			{Charge_integral,Consume_integral} = get(role_mall_integral),
			NewCharge_Integral = Charge_integral + MoneyCount,
			put(role_mall_integral,{NewCharge_Integral,Consume_integral}),
			mall_integral_db:add_role_mall_integral(get(roleid),NewCharge_Integral,Consume_integral),
			Message = login_pb:encode_change_role_mall_integral_s2c(#change_role_mall_integral_s2c{charge_integral=NewCharge_Integral,by_item_integral=Consume_integral}),
			% send_data_to_gate(Message),
			apply_component(send_to_gate_component,send_data_to_gate,[Message]),
			gm_logger_role:role_charge_integral_change(get(roleid),MoneyCount,NewCharge_Integral,Reason,get(level));
		(MoneyType =:= ?MONEY_CONSUMPTION_INTEGRAL) and (MoneyCount=/= 0)->
			{Charge_integral,Consume_integral} = get(role_mall_integral),
			NewConsum_Integral = Consume_integral + MoneyCount,
			put(role_mall_integral,{Charge_integral,NewConsum_Integral}),
			mall_integral_db:add_role_mall_integral(get(roleid),Charge_integral,NewConsum_Integral),
			Message = login_pb:encode_change_role_mall_integral_s2c(#change_role_mall_integral_s2c{charge_integral=Charge_integral,by_item_integral=NewConsum_Integral}),
			% send_data_to_gate(Message),
			apply_component(send_to_gate_component,send_data_to_gate,[Message]),
			gm_logger_role:role_consume_integral_change(get(roleid),MoneyCount,NewConsum_Integral,Reason,get(level));
		(MoneyType =:= ?MONEY_HONOR) and (MoneyCount=/= 0)->
			NewHonor = get_honor_from_roleinfo(RoleInfo) + MoneyCount,
			NewRoleInfo = set_honor_to_roleinfo(RoleInfo, NewHonor),
			NewAttrHonor = [{honor, NewHonor}],
			% only_self_update(NewAttrHonor),
			apply_component(role_update_broad_component,only_self_update,[NewAttrHonor]),
			put(creature_info, NewRoleInfo),
			gm_logger_role:role_honor_change(get(roleid),MoneyCount,NewHonor,Reason,get(level));
		(MoneyType =:= ?TYPE_GUILD_CONTRIBUTION) and (MoneyCount=/= 0)->
			case guild_util:is_have_guild() of
				true->
					guild_op:contribute(MoneyCount);
				_->
					ignor
			end;
		true->
			nothing
	end.


handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.