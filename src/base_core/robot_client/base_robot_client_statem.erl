-module(base_robot_client_statem).

-export([
	start/2,
	sendtoserver/2,
	kick_robot/1,
	kick_many_robot/1
]).

% 状态 loging、gaming、attacking
% -export([loging/2,gaming/2,attacking/2]).


-define(PACKAGE_INDEX,1000).
-define(ITEMNUM,10).
-define(MAP_X,200).
-define(MAP_Y,200).
-define(MAX_ATTACK_TIME,5).
%%-define(MOVE_SPEED,143).
%%-define(MOVE_SPEED,1000).
-define(MOVE_SPEED,2*?ROLE_ATTR_MOVESPEED).
-define(MOVE_NUM,6).
-define(MONEY,100000).
-define(GOLD,10000).
%% fashi,lieshou,zhanshi :1,2,3
%% fashi [713000001,713000002,713000003,713000004,713000005]
%% lieshou [712000001,712000002,712000003,712000004,712000005,712000006]
%% zhanshi[711000001,711000002,711000003,711000004,711000005,711000006]
-define(SKILLLIST,[[510000011],[520000011],[530000011]]).
%% -define(SKILLLIST,[[713000001,713000002,713000003,713000004,713000005],[712000001,712000002,712000003,712000004,712000005,712000006],[711000001,711000002,711000003,711000004,711000005,711000006]]).
-define(EQUIPMENT,[[12350541,12351041,12351141,12351241,12351341,12351441,12351541,12351641,12351741],[12350341,12351041,12351141,12351241,12351341,12351441,12351541,12351641,12351741],[12350141,12351041,12351141,12351241,12351341,12351441,12351541,12351641,12351741]]).

-record(state, {socket, role_info, map_info, client_config}).
-record(account,{username,roleids,gold,qq_gold,local_gold,nickname,gender,first_login_ip,first_login_time,last_login_ip,last_login_time,login_days,is_yellow_vip,is_yellow_year_vip,yellow_vip_level,first_login_platform,login_platform}).
-include("base_gen_statem_shared.hrl").
-include("login_pb.hrl").
-include("base_robot_client_def.hrl").
-include("common_define.hrl").
-include("quest_define.hrl").
-include("creature_define.hrl").
-include("attr_keyvalue_define.hrl").
-include("data_struct.hrl").

start(Id, Client_config)->
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	base_logger_util:info_msg("Id:~p Client_config:~p~n", [Id, Client_config]),
	% ?base_gen_statem:start_link({local,Id},?MODULE, [Client_config], []).
	% ?base_gen_statem:start_link({local,Id}, ?MODULE, [Client_config], []).
	?base_gen_statem:start({local,Id},?MODULE, [Client_config], []).

sendtoserver(Pid,Binary)->
	?ZS_LOG(),
	Pid ! {sendtoserver,Binary}.

?init([Client_config]) ->
	base_logger_util:info_msg("Current pid:~p~n",[self()]),
	process_flag(trap_exit,true),
	try
		% load_map_sup:start_link(),
		login_pb:create_ets(),
		login_pb:init_ets()
	catch
		_:_-> ignor
	end,

	{A,B,C} = os:timestamp(),
	rand:seed(exsplus,{A,B,C}),
	?ZS_LOG(),
	#robot_client_config{server_addr=Server_addr,server_port=Server_port} = Client_config,
	{ok,Socket} = gen_tcp:connect(Server_addr, Server_port, [binary,{packet,2}]),
	base_logger_util:info_msg("~p:~p connect Socket:~p~n",[?MODULE,?FUNCTION_NAME,Socket]),
	?base_gen_statem:cast(self(), {login}),
	?ZS_LOG(),
	% {ok,testing,#state{socket=Socket, role_info=B, map_info=C,client_config=Client_config}}.
	% {ok,testing,#state{socket=Socket,client_config=Client_config}}.
	{ok,loging,#state{socket=Socket,client_config=Client_config}}.


?handle_event(cast,{login},loging,State) ->
% loging({login}, State) ->
	base_logger_util:info_msg("~p:~p({login})~n",[?MODULE,?FUNCTION_NAME]),
	#state{client_config=Client_config} = State,
	?ZS_LOG(),
	{A,B,C} = os:timestamp(),
	?ZS_LOG(),
	#robot_client_config{user_name=Index, 
		       server_addr=Server_addr,
		       server_port=Server_port,
				lineid = LineId,
				mapid = MapId,
				level = Level,
				speekrate = SpeekRate,
				serverid = ServerId} = Client_config,
	?ZS_LOG(),
	rand:seed(exsplus,{A,B+Index,C}),
	?ZS_LOG(),
	User_name ="1000" ++ integer_to_list(Index),
	put(user_name,User_name),
	put(lineid ,LineId),
	put(mapid,MapId),
	put(speek_rate,SpeekRate),
	put(level,Level),
	put(in_battle,false),
	put(attack_time,0),
	?ZS_LOG(),
	Gender = (Index rem 2),
	put(gender,Gender),
	Class = (Index rem 3) + 1,
	put(class,Class),
	?ZS_LOG(),
	%% 认证开始
	UserId = integer_to_list(Index),
	put(userid,UserId),
	?ZS_LOG(),
	begin_auth(User_name,UserId,ServerId),
	?ZS_LOG(),
	put(player_list, []),
	?ZS_LOG(),
	{next_state, loging, State};
?handle_event(cast,#user_auth_fail_s2c{reasonid = ReasonId},loging,State)->
% loging(#user_auth_fail_s2c{reasonid = ReasonId}, State)->
	io:format("user_auth_fail_s2c reasonid:~p~n",[ReasonId]),
	{next_state, loging, State};
?handle_event(cast,#init_random_rolename_s2c{bn=Bname,gn=Gname},loging,State)->
% loging(#init_random_rolename_s2c{bn=Bname,gn=Gname},State)->
	base_logger_util:info_msg("~p:~p(init_random_rolename_s2c)~n",[?MODULE,?FUNCTION_NAME]),
	%%io:format("a new player ~n"),
	Gender = get(gender),
	if
		Gender =:= 0 ->
			if
				is_binary(Gname)->
					put(role_name,binary_to_list(Gname));
				true->
					put(role_name,Gname)
			end;
		true->
			if
				is_binary(Bname)->
					put(role_name,binary_to_list(Bname));
				true->
					put(role_name,Bname)
			end
	end,
	send_create_role_request(),
	{next_state, loging, State};


?handle_event(cast,#player_role_list_s2c{roles=RoleList},loging, State)->
% loging(#player_role_list_s2c{roles=RoleList}, State)->
	base_logger_util:info_msg("~p:~p(player_role_list_s2c)~n",[?MODULE,?FUNCTION_NAME]),
	case RoleList of
		[]->
			nothing;
		_-> 
		%%	io:format("player_role_list_s2c rolelist:~p~n",[RoleList]),
			[#r{roleid=RoleId, 
						name=RoleName,
						lastmapid=LastMapId,
						classtype = Classtype,
						gender = Gender
					}|_T] = RoleList,
			put(role_id,RoleId),
			put(gender,Gender),
			put(class,Classtype),
			put(role_name,RoleName),
			if
				is_binary(RoleName)->
					put(role_name,binary_to_list(RoleName));
				true->
					put(role_name,RoleName)
			end,
			{A,B,C} = os:timestamp(),
			rand:seed(exsplus,{A,B+RoleId,C}),
			send_line_query(LastMapId)
	end,
	{next_state, loging, State};
 
?handle_event(cast,#create_role_sucess_s2c{role_id=RoleId},loging, State)->
% loging(#create_role_sucess_s2c{role_id=RoleId}, State)->
	base_logger_util:info_msg("~p:~p(create_role_sucess_s2c) RoleId:~p~n",[?MODULE,?FUNCTION_NAME,RoleId]),
	put(role_id,RoleId),
	send_role_select(get(role_id),get(lineid)),
	{next_state, loging, State};

?handle_event(cast,#role_line_query_ok_s2c{lines = Lines},loging, State)->
% loging(#role_line_query_ok_s2c{lines = Lines}, State)->
	base_logger_util:info_msg("~p:~p(role_line_query_ok_s2c)~n",[?MODULE,?FUNCTION_NAME]),
	send_role_select(get(role_id),get(lineid)),
	{next_state, loging, State};
	
?handle_event(cast,#learned_skill_s2c{skills = SkillsList},loging, State)->
% loging(#learned_skill_s2c{skills = SkillsList}, State)->
	base_logger_util:info_msg("~p:~p(learned_skill_s2c) SkillsList~p~n",[?MODULE,?FUNCTION_NAME,SkillsList]),
	Skills =
		if
			SkillsList =:= []->
%% 				io:format("learned_skill_s2c"),
				[];
			true->
				lists:map(fun({_,SkillId,_,_})->
								  SkillId
						  end,SkillsList)
		end,
	put(roleskill,Skills),
	{next_state, loging, State};
	
?handle_event(cast,#role_map_change_s2c{x=LX, y=LY,lineid = LastLineId,mapid =LastMapid},loging, State)->
% loging(#role_map_change_s2c{x=LX, y=LY,lineid = LastLineId,mapid =LastMapid}, State)->
	base_logger_util:info_msg("~p:~p(role_map_change_s2c)~n",[?MODULE,?FUNCTION_NAME]),
	send_map_complete(),
	TargetMap = get(mapid),
	TargetLine = get(lineid),
%% 	io:format("loging role_map_change_s2c,Lineid:~p,mapid:~p,TargetLine:~p,TargetMap:~p~n",[LastLineId,LastMapid,TargetLine,TargetMap]),
	IsMapLine = (TargetMap =:= LastMapid) and (TargetLine =:= LastLineId),
	if
		IsMapLine ->
		%%	io:format("roleid:~p,loging  send_map_complete~n",[get(role_id)]),
			put(pos,{LX,LY}),
			put(path,[]),
			gm_levelup(get(level)),
			gm_moneychange(?MONEY),
			gm_goldchange(?GOLD),
 			timer:send_afr(rand:uniform(60)*1000,{speek_loop}),
			self() ! {check_alive},
			query_time_c2s(),
			{next_state, gaming, State};
		TargetMap =/= LastMapid->
		%%	io:format("roleid:~p, loging TargetMap:~p,LastMapid:~p~n",[get(role_id),TargetMap,LastMapid]),
			{Random_X,Random_Y} = path:random_pos(TargetMap),
			put(pos,{Random_X,Random_Y}),
			gm_move(TargetMap,Random_X,Random_Y),
			{next_state, loging, State};
		true->
		%%	io:format("roleid:~p, loging TargetLine:~p,LastLineId:~p~n",[get(role_id),TargetLine,LastLineId]),
			change_line(TargetLine),
			{next_state, loging, State}
	end;

% loging(_Other, State)->
% 	base_logger_util:info_msg("~p:~p(_Other)~n",[?MODULE,?FUNCTION_NAME]),
% 	ignor,
% 	{next_state, loging, State}.

?handle_event(cast,#update_skill_s2c{skillid = SkillId},gaming,State)->
% gaming(#update_skill_s2c{skillid = SkillId},State)->
%% 	io:format("update_skill_s2c gaming,skillid:~p~n",[SkillId]),
	case lists:member(SkillId,get(roleskill)) of
		false->
			put(roleskill,[SkillId|get(roleskill)]);
		true->
			nothing
	end,
	{next_state, gaming, State};


?handle_event(cast,#role_map_change_s2c{x=X, y=Y,lineid = LineId,mapid =Mapid}, gaming,State)->
% gaming(#role_map_change_s2c{x=X, y=Y,lineid = LineId,mapid =Mapid}, State)->
	%%io:format("gaming,roleid:~p,Line:~p,,Mapid:~p~n,X:~p,Y:~p~n",[get(role_id),LineId,Mapid,X,Y]),
	send_map_complete(),
	set_pkmodel(3),
	start_random_move(),
	{next_state, gaming, State};


?handle_event(cast,#query_time_s2c{time_async = ServerTime},gaming,State)->
% gaming(#query_time_s2c{time_async = ServerTime},State)->
%% 	io:format("query_time_s2c~n"),
	put(server_time,{os:timestamp(),ServerTime - 50}),
	SkillNum = erlang:length(get(roleskill)),
	if 
		SkillNum=<1->
			ClassSkill = lists:nth(get(class),?SKILLLIST),
			learn_skill(ClassSkill);
		true->
			nothing
	end,
	clear_package(),
	EquipmentList = lists:nth(get(class),?EQUIPMENT),
	RandomNum = rand:uniform(erlang:length(EquipmentList)),
	Equipment = lists:nth(RandomNum,EquipmentList),
	gm_getitem(Equipment),
	start_random_move(),
	{next_state, gaming, State};


?handle_event(cast,#add_item_s2c{item_attr = #i{slot = Slot}}, gaming,State)->	
% gaming(#add_item_s2c{item_attr = #i{slot = Slot}}, State)->	
	Msg = login_pb:encode_proto_msg(auto_equip_item_c2s,#auto_equip_item_c2s{slot = Slot}),
	sendtoserver(self(), Msg),
	gm_randommap(),
	{next_state, gaming, State};


?handle_event(cast,#questgiver_states_update_s2c{npcid = Npcs,queststate = States}, gaming,State)->
% gaming(#questgiver_states_update_s2c{npcid = Npcs,queststate = States}, State)->
	case lists:member(1,Npcs) of
		true->
			QuestState = lists:foldl(fun(Index,Tmpre)->
						if
							Tmpre =/= 0->
								Tmpre;
							true->
								case lists:nth(Index,Npcs) of
									2010001->
										lists:nth(Index,States);
									_->
										Tmpre 
								end
						end
					end,0,lists:seq(1, erlang:length(Npcs)) ),
			if
				QuestState =:= ?QUEST_STATUS_COMPLETE->
					%%io:format("questgiver_states_update_s2c0000000~n"),
					put(next_action,{com_quest,2010001}),
					put(path,[]);
				true->
					nothing
			end;
		_->
			nothing
	end,
	{next_state, gaming, State};

?handle_event(cast,#be_killed_s2c{creatureid = CreatureId}, gaming,State)->
% gaming(#be_killed_s2c{creatureid = CreatureId}, State)->
	%%io:format("be_killed_s2c creatureid~p  get(role_id) ~p~n",[CreatureId, get(role_id)]),
	MyId = get(role_id),
	MyTarget = get(target),
	case CreatureId of
		MyTarget->
			put(target,0),
			put(next_action,random_move),
			start_random_move();
		MyId->
			Msg = login_pb:encode_proto_msg(role_respawn_c2s,#role_respawn_c2s{type = 2}),
			sendtoserver(self(), Msg),
			put(target,0),
			start_random_move();
		_->
			nothing
	end,
	{next_state, gaming , State};
	

?handle_event(cast,#object_update_s2c{create_attrs = NewCommers,change_attrs = _Change, deleteids = Deletes}, gaming,State)->
% gaming(#object_update_s2c{create_attrs = NewCommers,change_attrs = _Change, deleteids = Deletes}, State)->
	change_my_aoi(NewCommers,Deletes),
	{next_state, gaming, State};

?handle_event(cast,#role_move_fail_s2c{pos = #c{x=TmpX, y=TmpY} }, gaming,State)->
% gaming(#role_move_fail_s2c{pos = #c{x=TmpX, y=TmpY} }, State)->
	put(pos,{TmpX,TmpY}),
	put(path,[]),
	put(target,0),
	start_random_move(),
	{next_state, gaming, State};

%%战场开启
?handle_event(cast,#battle_start_s2c{type = Type,lefttime = LeftTime},gaming,State)->
% gaming(#battle_start_s2c{type = Type,lefttime = LeftTime},State)->
	case get(in_battle) of
		true->
			nothing;
		_->
			timer:send_after(rand:uniform(60)*1000,{join_battle})
	end,
	{next_state, gaming, State};

%%战场结束
?handle_event(cast,#battle_end_s2c{},gaming,State)->
% gaming(#battle_end_s2c{},State)->
	case get(in_battle) of
		true->
			put(in_battle,false),
			Msg = login_pb:encode_proto_msg(battle_leave_c2s,#battle_leave_c2s{}),
			sendtoserver(self(), Msg),
%% 			put(last_random_move,os:timestamp()),
			start_random_move();
		_->
			nothing
	end,
	{next_state, gaming, State};
	
?handle_event(cast,{move_heartbeat,ReqList},gaming,State)->
% gaming({move_heartbeat,ReqList},State)->
	case ReqList =/= [] of 		%%寻路未跑完
		true->
			[#c{x = X,y=Y}|T] = ReqList,
			put(pos,{X,Y}),
			erlang:send_after(2000,self(),{move_heartbeat, T}),
%% 			erlang:send_after(?MOVE_SPEED*3,self(),{move_heartbeat, T}),
			NextState = gaming;
		_->			%%格跑完
			case get(path) of
				[]->			%%路径跑完,做行动
					{PosX,PosY} = get(pos),
					%%io:format("stop_move_c2s ~p ~n",[get_now_time()]),
					Msg = login_pb:encode_proto_msg(stop_move_c2s,#stop_move_c2s{posx = PosX,posy = PosY,time = get_now_time()}),
					sendtoserver(self(),Msg), 
					case get(next_action) of
						random_move->
							range_alert(),
							NextState = gaming;%%战斗寻路或者继续随机寻路跑
						Other->
							case Other of
								{com_quest,2010001}->
								case get(pos) of
									{38,76}->
										com_quest(2010001),
										NextState = gaming;
									_->
										start_move({38,76}),
										NextState = gaming
								end;
								{attack,Objectid,_}->
									start_attack(Objectid),
									NextState = attacking
							end
					end;
				Path->				%%路径没跑完,继续跑
					move_request(Path),
					NextState = gaming
			end
	end,
	{next_state, NextState, State};
	
?handle_event(cast,#other_role_move_s2c{other_id=OtherId,posx=X,posy=Y},gaming,State)->
% gaming(#other_role_move_s2c{other_id=OtherId,posx=X,posy=Y},State)->
	put(aoi_list,[{OtherId,2,{X,Y}}|get(aoi_list)]),
	{next_state, gaming, State};

?handle_event(cast,#be_attacked_s2c{enemyid=EnemyId,skill=_Skill,units=_Units,flytime=_Flytime},gaming,State)->
% gaming(#be_attacked_s2c{enemyid=EnemyId,skill=_Skill,units=_Units,flytime=_Flytime},State)->
%% 	Msg = "你丫有毛病!见人就砍?哪里走,看招!",
%% 	Message = login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = 1, desserverid = 1, desrolename = "", msginfo = Msg, details=[0],reptype=0}),
%% 	sendtoserver(self(), Message),
	[Skill|_]=get(roleskill),
	robot_attack(Skill,EnemyId),
	{next_state,gaming, State};

% gaming(_Other, State)->
% %% 	io:format("gaming,Other:~p~n",[_Other]),
% 	{next_state, gaming, State}.

?handle_event(cast,{attack_heartbeat,CreatureId}, attacking,State)->
% attacking({attack_heartbeat,CreatureId}, State)->
 	%%io:format("attack_heartbeat begin ~n"),
	SkillList = get(roleskill),
	Random = rand:uniform(erlang:length(SkillList)),
	SkillId = lists:nth(Random,SkillList),
	Message = login_pb:encode_proto_msg(role_attack_c2s,#role_attack_c2s{creatureid=CreatureId, skillid=SkillId}),
	sendtoserver(self(), Message),
	erlang:send_after(1300,self(),{attack_heartbeat,CreatureId}),
	{next_state, attacking, State};

?handle_event(cast,#be_killed_s2c{creatureid = CreatureId}, attacking,State)->
% attacking(#be_killed_s2c{creatureid = CreatureId}, State)->
 	%%io:format("be_killed_s2c creatureid~p  get(role_id) ~p~n",[CreatureId, get(role_id)]),
	MyId = get(role_id),
	case CreatureId of
		MyId->
			my_respawn();
		_Other->
			nothing
	end,
	put(target,0),
	start_random_move(),
	{next_state, gaming  , State};

?handle_event(cast,#role_attack_s2c{result = Result,enemyid = CastId}, attacking,State)->
% attacking(#role_attack_s2c{result = Result,enemyid = CastId}, State)->
	%%io:format("role_attack_s2c Result:~p,curpos:~p~n",[Result,get(pos)]),
	case (get(role_id)=:= CastId) and (Result =/= 0) and (Result =/= 10012)of
		true->
			%%io:format("role_attack_s2c Error Result ~p~n",[Result]),
			put(target,0),
			NextState = gaming,
			range_alert();
			%%start_random_move();
		_->
			NextState  = attacking
	end,
	{next_state, NextState, State};


% attacking(_Other, State)->
% 	ignor,
% 	{next_state, attacking, State}.

?handle_event(info, {quit},StateName, State) ->
% handle_info({quit},StateName, State) ->
	?ZS_LOG(),
	#state{client_config=Client_config,socket=Socket} = State,    
	gen_tcp:close(Socket),
	timer:send_after(2000, {init}),
	{next_state, StateName, State};

?handle_event(info, {check_alive},StateName, State) ->
% handle_info({check_alive},StateName, State) ->
	?ZS_LOG(),
	case get(check_alive) of
		undefined->
			put(check_alive,os:timestamp());
		Timer->
			DealyTime = timer:now_diff(os:timestamp(),Timer),
			if 
				DealyTime > 10000*1000-> 				%%10s
					nothing;
%%					 io:format("tcp not alive check_alive DealyTime ~p seconds~n",[DealyTime/1000000]);
%% 					 ?base_gen_statem:cast(self(), {restart});
				true->
					nothing
			end
	end,
	timer:send_after(10000,{check_alive}),
	{next_state, StateName, State};

?handle_event(info, {speek_loop},StateName,State)->
% handle_info({speek_loop},StateName,State)->
	?ZS_LOG(),
	case rand:uniform(100) > get(speek_rate) of
		true->
			nothing;
		_->
			speek_to_world()
	end,
	timer:send_after(10000+rand:uniform(20000),{speek_loop}),
%% 	timer:send_after(10000,{speek_loop}),
	{next_state, StateName, State};

?handle_event(info, {sendtoserver,Binary},StateName,#state{socket=Socket}=State)->
% handle_info({sendtoserver,Binary},StateName,#state{socket=Socket}=State)->
	?ZS_LOG(),
	base_logger_util:info_msg("~p:~p({sendtoserver,Binary=~p},StateName=~p,State=~p)~n)",[?MODULE,?FUNCTION_NAME,Binary,StateName,State]),
	gen_tcp:send(Socket, Binary),
	{next_state, StateName, State};
	
?handle_event(info, {tcp_closed, _Socket}, StateName, StateData) ->
% handle_info({tcp_closed, _Socket}, StateName, StateData) ->
	?ZS_LOG(),
	io:format("tcp_closed Roleid ~p~n",[get(role_id)]),
	exit(normal),
	{stop,normal, StateData};

?handle_event(info, {tcp,Socket,Binary},StateName,State)->
% handle_info({tcp,Socket,Binary},StateName,State)->
	?ZS_LOG(),
	try
		put(check_alive,os:timestamp()),
		%@@Term = erlang:binary_to_term(Binary),
		%@@ID = element(2,Term),
		%@@BinMsg = erlang:setelement(1,Term, login_pb:get_record_name(ID)),
		<<ID:16, Binary0/binary>> = Binary,
		RecordName = login_pb:get_record_name(ID),
		base_logger_util:info_msg("~p:~p({tcp,Socket,Binary}) proto:~p~n",[?MODULE,?FUNCTION_NAME,RecordName]),
		case RecordName of
			user_auth_fail_s2c ->
				BinMsg = login_pb:decode_user_auth_fail_s2c(Binary0);
			init_random_rolename_s2c->
				BinMsg = login_pb:decode_init_random_rolename_s2c(Binary0);
			player_role_list_s2c->
				BinMsg = login_pb:decode_player_role_list_s2c(Binary0);
			create_role_sucess_s2c->
				BinMsg = login_pb:decode_create_role_sucess_s2c(Binary0);
			role_line_query_ok_s2c->
				BinMsg = login_pb:decode_role_line_query_ok_s2c(Binary0);
			learned_skill_s2c->
				BinMsg = login_pb:decode_learned_skill_s2c(Binary0);
			role_map_change_s2c->
				BinMsg = login_pb:decode_role_map_change_s2c(Binary0);
			query_time_s2c->
				BinMsg = login_pb:decode_query_time_s2c(Binary0);
			add_item_s2c->
				BinMsg = login_pb:decode_add_item_s2c(Binary0);
			questgiver_states_update_s2c->
				BinMsg = login_pb:decode_questgiver_states_update_s2c(Binary0);
			be_killed_s2c->
				BinMsg = login_pb:decode_be_killed_s2c(Binary0);
			role_attack_s2c->
				BinMsg = login_pb:decode_role_attack_s2c(Binary0);
			quit->
				BinMsg = login_pb:decode_quit(Binary0);
			check_alive->
				BinMsg = login_pb:decode_check_alive(Binary0);
			speek_loop->
				BinMsg = login_pb:decode_speek_loop(Binary0);
			join_battle->
				BinMsg = login_pb:decode_join_battle(Binary0);
			other_role_move_s2c->
				BinMsg = login_pb:decode_other_role_move_s2c(Binary0);
			be_attacked_s2c->
				BinMsg = login_pb:decode_be_attacked_s2c(Binary0)
		end,
		?base_gen_statem:cast(self(), BinMsg)
	catch
		E:R->
			nothing%%slogger:msg("tcp error record_name Binary E:~p,R~p~n",[E,R])
	end,
	inet:setopts(Socket, [{active, once}]),
	{next_state, StateName, State};

?handle_event(info, {join_battle},StateName,State)->
% handle_info({join_battle},StateName,State)->
	?ZS_LOG(),
	case get(in_battle) of
		true->
			nothing;
		_->
			put(in_battle,true),
			Msg = login_pb:encode_proto_msg(battle_join_c2s,#battle_join_c2s{type = ?TANGLE_BATTLE}),
			sendtoserver(self(), Msg)
	end,
	{next_state, StateName, State};	

?handle_event(info, {continue_attack,{Skill,TargetId}},StateName,State)->
% handle_info({continue_attack,{Skill,TargetId}},StateName,State)->
	?ZS_LOG(),
	robot_attack(Skill,TargetId),
	{next_state, StateName, State};

% ==================================handle undefined event============================================
?handle_event({call, From}, EventContent, StateName, StateData) ->
	base_logger_util:info_msg("~p handle_event undefined event:({call, From:~p}, EventContent:~p, StateName:~p, StateData:~p) !!!!!~n",[?MODULE, From, EventContent, StateName, StateData]),
	{keep_state_and_data, [{reply, From, ok}]};
?handle_event(Event, EventContent, StateName, StateData) ->
	base_logger_util:info_msg("~p handle_event undefined event:(Event:~p, EventContent:~p, StateName:~p, StateData:~p) !!!!!~n",[?MODULE, Event, EventContent, StateName, StateData]),
	keep_state_and_data.
% ==================================handle undefined event============================================

handle_server_message(#other_role_move_s2c{})->	
	nothing;

handle_server_message(IgnorMessage)->
	io:format("IgnorMessage ~p~n",[IgnorMessage]).
	
send_quest_compelete()->
	todo.
	
move_to_target(NpcId)->
	todo.

change_my_aoi(NewCommers,Deletes)->
	lists:foreach(fun(#o{objectid = Objectid,objecttype = _Type,attrs = _Attrs})->
						put(aoi_list,lists:keydelete(Objectid,1,get(aoi_list)))
				 end,Deletes),
	Npcs = lists:foldl(fun(#o{objectid = Objectid,objecttype = Type,attrs = Attrs}, NpcTmps)->
					{_,_,X} = lists:keyfind(?ROLE_ATTR_POSX,2,Attrs),
					{_,_,Y} = lists:keyfind(?ROLE_ATTR_POSY,2,Attrs),
					case lists:keyfind(?ROLE_ATTR_CREATURE_FLAG,2,Attrs) of
						false->
							NpcTmps;
						{_,_,CreatureType} ->
							put(aoi_list,[ {Objectid,CreatureType,{X,Y}}| get(aoi_list)]),
							if
								(Type =:= ?UPDATETYPE_NPC) and(CreatureType =:= ?CREATURE_NPC)->
									[ Objectid|NpcTmps ];
								true->
									NpcTmps
							end
					end
			end,[],NewCommers),
	if
		Npcs =/= []->
			send_query_npc_state(Npcs);
		true->
			nothing
	end. 
	
range_alert()->
	case get(map_id) of 
		101->
			nothing;
		_->
			MonsterAoi = lists:filter(fun({_,TypeTmp,_})->
					TypeTmp =:= ?CREATURE_MONSTER
				end,get(aoi_list)),
			Length = erlang:length(MonsterAoi),
			if
				Length > 0->
					Nth = rand:uniform(Length),
					{Objectid1,_,Pos1} = lists:nth(Nth,MonsterAoi),
					case get(target) of
						0->
							put(target,Objectid1),
							put(next_action,{attack,Objectid1,Pos1});
						_->
							nothing
					end;
				true->
					nothing
			end,
			%%
			%%战场中target可能是玩家
			%%
			case ( (get(target) =:= 0) and get(in_battle) ) of
				true->
					RoleAoi = lists:filter(fun({_,TypeTmp,_})->
						TypeTmp =:= ?UPDATETYPE_ROLE
					end,get(aoi_list)),
					Length1 = erlang:length(RoleAoi),
					if
						Length1 > 0-> 
							Nth1 = rand:uniform(Length1),
							{Objectid2,_,Pos2} = lists:nth(Nth1,RoleAoi),
							case get(target) of
								0->
									put(target,Objectid2),
									put(next_action,{attack,Objectid2,Pos2});
								_->
									nothing
							end;
						true->
							nothing
					end;
				_->
					RoleAoi = lists:filter(fun({_,TypeTmp,_})->
						TypeTmp =:= ?CREATURE_MONSTER
					end,get(aoi_list)),
					Length1 = erlang:length(RoleAoi),
					if
						Length1 > 0-> 
							Nth1 = rand:uniform(Length1),
							{Objectid2,_,Pos2} = lists:nth(Nth1,RoleAoi),
							case get(target) of
								0->
									put(target,Objectid2),
									put(next_action,{attack,Objectid2,Pos2});
								_->
									nothing
							end;
						true->
							nothing
					end
%% 					nothing
			end
	end,

	case get(target) of
		0->				%%没有敌人,继续跑
			case get(path) of
				[]->
					start_random_move();
				Path->
					move_request(Path)
			end;
		_->
			case get(next_action) of
				{attack,_Objectid,Pos}->
					[Skill|_]=get(roleskill),
					robot_attack(Skill,_Objectid),
 					start_move(Pos);
				_->
					put(target,0)
			end
	end.
	
send_query_npc_state(NpcIds)->
	Message = login_pb:encode_proto_msg(questgiver_states_update_c2s,#questgiver_states_update_c2s{npcid = NpcIds}),
	sendtoserver(self(), Message).
	
send_create_role_request()->
	Name = get(role_name),
	Gender = get(gender),
	Class = get(class),
	%%io:format("send_create_role_request Gender ~p Class ~p ~n",[Gender,Class]),
	Message = login_pb:encode_proto_msg(create_role_request_c2s,#create_role_request_c2s{role_name = Name,gender = Gender,classtype = Class}),
	sendtoserver(self(), Message).

send_line_query(LastMapId)->
	Message = login_pb:encode_proto_msg(role_line_query_c2s,#role_line_query_c2s{mapid = LastMapId}),
	sendtoserver(self(), Message).

send_role_select(RoleId,LineId)->
	Message = login_pb:encode_proto_msg(player_select_role_c2s,
						#player_select_role_c2s{roleid = RoleId,lineid = LineId}),
	sendtoserver(self(), Message).

send_map_complete()->
	Message = login_pb:encode_proto_msg(map_complete_c2s,#map_complete_c2s{}),
	sendtoserver(self(), Message ).

query_time_c2s()->
	Message2 = login_pb:encode_proto_msg(query_time_c2s,#query_time_c2s{}),
	sendtoserver(self(), Message2 ),
	put(target,0),
	put(aoi_list,[]).

change_line(TargetLine)->
	Message = login_pb:encode_proto_msg(role_change_line_c2s,#role_change_line_c2s{lineid = TargetLine}),
	sendtoserver(self(), Message).
	
	
start_random_move() ->
	put(next_action,random_move),
	put(target,0),
	{X,Y} = path:random_pos(get(mapid)),
	CurPos = get(pos),
	TmpPath = path:path_find(CurPos,{X,Y},get(mapid)),
%% 	io:format("tmppath:~p, ~p~n ~p~n,curpos:~p,X,Y:~p~n",[get(userid), get(role_id), TmpPath,CurPos,{X,Y}]),
%% 	case timer:now_diff(os:timestamp(),get(last_random_move))/1000>=3000 of
%% 		true->
	if
		TmpPath =:= []->
			if
				CurPos =:={X,Y}->
					start_random_move();
				true->
				%%	io:format("start_random_move curpos:~p,targetpos:~p~n",[CurPos,{X,Y}]),
					{NX,NY} = path:random_pos(get(mapid)),
					put(pos,{NX,NY}),
					gm_move(get(mapid),NX,NY)
			end;
		true->
			Path = lists:map(fun({TmpX,TmpY})-> #c{x=TmpX, y=TmpY} end,TmpPath),
 			put(path,Path),
			move_request(Path)
	end.
%% 		_->nothing
%% 	end.
	
start_move(Pos) ->
	CurPos = get(pos),
 	FindPath = path:path_find(CurPos,Pos,get(mapid)),
	if
		FindPath =:= []->
			if
				Pos =:= CurPos->
					%%io:format("start_move curpos:~p,targetpos:~p~n",[CurPos,Pos]),
					?base_gen_statem:cast(self(), {move_heartbeat,[]});
				true->
%% 					io:format("start_move curpos:~p,targetpos:~p~n",[CurPos,Pos]),
					start_random_move()
			end;
		true->
			Path1 = lists:map(fun({TmpX,TmpY})-> #c{x=TmpX, y=TmpY} end,FindPath),
			put(path,Path1),
			move_request(Path1)
	end.

move_request(Path)->
	{ReqList,RemList} = lists:split(erlang:min(?MOVE_NUM,erlang:length(Path)),Path),
	put(path,RemList),
	if
		Path =/= []->
			[NextPos|_RePath] =  ReqList,
			{_,X,Y} = NextPos,
			{NowX,NowY} = get(pos),
			put(pos,{X,Y}),
			Message = login_pb:encode_proto_msg(role_move_c2s,#role_move_c2s{time = get_now_time(),posx = NowX,posy = NowY,path=ReqList}),
			sendtoserver(self(), Message);
		true->
			nothing
	end,
	?base_gen_statem:cast(self(), {move_heartbeat,ReqList}).

get_now_time()->
	{Now,ServerTime } = get(server_time),
	trunc(timer:now_diff(os:timestamp(),Now)/1000) +ServerTime.

com_quest(2010001)->
	%%io:format("com_quest 2010001~n"),
	Message = login_pb:encode_proto_msg(questgiver_complete_quest_c2s,#questgiver_complete_quest_c2s{npcid = 2010001,questid = 31401000,choiceslot = 0}),
	sendtoserver(self(), Message).

start_attack(CreatureId) ->
	?base_gen_statem:cast(self(), {attack_heartbeat,CreatureId}).

begin_auth(AccountName,UserId,ServerId)->
	SecretKey = "E3it45tiOjLi&fie8Hje56uMu67h",
	{MegaSecs, Secs, _MicroSecs} = os:timestamp(),
	TimeSeconds = Secs+MegaSecs*1000000,
	Time = integer_to_list(TimeSeconds),
	BinName = case is_binary(AccountName) of
			  	  true-> AccountName;
				   _-> list_to_binary(AccountName)
			  end,
	NameEcode = base_auth_util:escape_uri(BinName),
	ValStr = UserId
					 ++ NameEcode ++ Time
					 ++ SecretKey ++ "1",
	MD5Bin = erlang:md5(ValStr),
	Md5Str = base_auth_util:binary_to_hexstring(MD5Bin),
	AuthTerm = #user_auth_c2s{username=AccountName,userid=UserId,time=Time,cm="1",serverid = ServerId,flag = Md5Str,userip = "",type = "",sid = "",openid="",openkey="",appid="",pf="",pfkey=""},
	?ZS_LOG(),
	Binary = login_pb:encode_proto_msg(user_auth_c2s,AuthTerm),
	?ZS_LOG(),
	sendtoserver(self(), Binary).

set_robot_gmaccount(RobotName)->
	DbNodes = base_node_util:get_dbnode(),
	case rpc:call(DbNodes,gm_op,gm_set_role_privilege_rpc,[RobotName,3]) of
		{error,"norole"}->
			nothing;
		{ok}->
			nothing;
		_->
			nothing
	end.

kick_robot(RobotAccount)->
	case dal:read_rpc(account,RobotAccount) of
		{ok,[]}->
			nothing;
		{ok,[Info]}->
			[RobotId]=Info#account.roleids,
			[MapNode |_] = base_node_util:get_mapnodes(),
			case rpc:call(MapNode,gm_order_op,kick_user,[RobotId]) of
				{error,"notonline"}->
					notonline;
				{ok}->
					ok;
				_->
					nothing
			end;
		_->
			nothing
	end.

kick_many_robot(Num) when Num>=1->
	case dal:read_rpc(account) of
		{ok,[]}->
			nothing;
		{ok,InfoL}->
			KickList=lists:foldl(fun(Info,Acc)->
										 Account=Info#account.username,
										 [Id]=Info#account.roleids,
										 Re=(string:str(Account,"1000")=:=1),
										 if
											 Re->
												 case role_pos_util:where_is_role(Id) of
													 []->
														 Acc;
													 _Pos->
												 [Account]++Acc
												 end;
											 true->
												 Acc
										 end
								 end,[],InfoL),
			case KickList of
				[]->
					nothing;
				_->
					NewNum=erlang:min(Num,length(KickList)),
					lists:foreach(fun(N)->
										  NAccount=lists:nth(N,KickList),
										  kick_robot(NAccount)
								  end,lists:seq(1,NewNum))
			end;
		_->
			nothing
	end;
					
kick_many_robot(Num) when Num<1 ->
nothing.

call_robot(Index)->
	if
		Index>1500->
			nothing;
		true->
			try
				gen_server:cast(load_map_process,{call_robot,Index})
			catch
				E:R->
					slogger:msg("Error = ~p,Reason = ~p~n",[E,R]),
					nothing
			end
	end.

set_pkmodel(Type)->
	Message=login_pb:encode_proto_msg(set_pkmodel_c2s,#set_pkmodel_c2s{pkmodel=Type}),
	sendtoserver(self(), Message).

robot_attack(Skill,TargetId)->
	Message=login_pb:encode_proto_msg(role_attack_c2s,#role_attack_c2s{skillid=Skill,creatureid=TargetId}),
	sendtoserver(self(), Message),
	self()!{continue_attack,{Skill,TargetId}}.

gm_randommap()->
	{MapId,X,Y} = lists:nth(rand:uniform(10),[{100,39,78},{200,64,122},{300,175,175},{500,81,89},{600,206,176},{700,82,128},{1000,88,101},{1300,64,56},{1400,71,133},{333,140,144}]),
	put(mapid,MapId),
	Msg = ".zymove "++erlang:integer_to_list(MapId)++" "++erlang:integer_to_list(X)++" "++erlang:integer_to_list(Y),
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = ?CHAT_TYPE_INTHEVIEW, desserverid=0, desrolename = "", msginfo = Msg, details="",reptype=0}),
	sendtoserver(self(), Message).

gm_move(MapId,X,Y)->
	Msg = ".zymove "++erlang:integer_to_list(MapId)++" "++erlang:integer_to_list(X)++" "++erlang:integer_to_list(Y),
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = ?CHAT_TYPE_INTHEVIEW, desserverid=0, desrolename = "", msginfo = Msg, details="",reptype=0}),
	sendtoserver(self(), Message).

gm_levelup(Level)->
	Msg = ".zylevelup "++erlang:integer_to_list(Level),
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = ?CHAT_TYPE_INTHEVIEW, desserverid=0, desrolename = "", msginfo = Msg, details="",reptype=0}),
	sendtoserver(self(), Message).

gm_moneychange(Money)->
	Msg = ".zymoney "++erlang:integer_to_list(Money),
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = ?CHAT_TYPE_INTHEVIEW, desserverid=0, desrolename = "", msginfo = Msg, details="",reptype=0}),
	sendtoserver(self(), Message).

gm_goldchange(Gold)->
	Msg = ".zygold "++erlang:integer_to_list(Gold),
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = ?CHAT_TYPE_INTHEVIEW, desserverid=0, desrolename = "", msginfo = Msg, details="",reptype=0}),
	sendtoserver(self(), Message).

%%获得物品
gm_getitem(ItemId)->
	Msg = ".zyitem "++erlang:integer_to_list(ItemId),
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = ?CHAT_TYPE_INTHEVIEW, desserverid=0, desrolename = "", msginfo = Msg, details="",reptype=0}),
	sendtoserver(self(), Message).

%%喊话
speek_to_world()->
	Level = get(level),
	if 
		Level =< 10->
			Type = ?CHAT_TYPE_INTHEVIEW;
		true->
			Type = ?CHAT_TYPE_WORLD
	end,
	Msg = robot_speak:get_random_word(),
%% 	Msg = "我是"++ get(role_name)++" ++好友 ",
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = Type , desserverid = 1, desrolename = "", msginfo = Msg, details=[0],reptype=0}),
	sendtoserver(self(), Message).


my_respawn()->
	Msg = ".zyitem 19000270",
	Message =  login_pb:encode_proto_msg(chat_c2s,#chat_c2s{type = ?CHAT_TYPE_INTHEVIEW, desserverid = 1,desrolename ="", msginfo = Msg, details="",reptype=0}),
	sendtoserver(self(), Message),
	Msg1 = login_pb:encode_proto_msg(role_respawn_c2s,#role_respawn_c2s{type = 2}),
	sendtoserver(self(), Msg1).

%%学习技能
learn_skill([])->
	nothing;
learn_skill([Skillid|ReMainSkill])->
	Message = login_pb:encode_proto_msg(skill_learn_item_c2s,#skill_learn_item_c2s{skillid = Skillid}),
	sendtoserver(self(), Message),
	learn_skill(ReMainSkill).

%%清理背包
clear_package()->
	destroy_item(?PACKAGE_INDEX+1,?ITEMNUM).
destroy_item(_PackageSlot,0)->
	nothing;
destroy_item(PackageSlot,ItemNum)->
	Message = login_pb:encode_proto_msg(destroy_item_c2s,#destroy_item_c2s{slot = PackageSlot}),
	sendtoserver(self(), Message),
	destroy_item(PackageSlot+1,ItemNum-1).


%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
?terminate(Reason, StateName, StateData) ->
	base_logger_util:info_msg("process terminate Reason=~p StateName=~p StateData=~p~n",[Reason,StateName,StateData]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
?code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
