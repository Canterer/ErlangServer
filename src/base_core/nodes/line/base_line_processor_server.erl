%% Description: TODO: Add description to base_line_processor_server
-module(base_line_processor_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/1,
	lookup_map_name/2,
	do_regist/3,
	unregist_by_node/1,
	get_map/3
]).
-export([
	get_role_count_by_map/1,
	get_role_num_by_mapId/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(LOOKUP_INTERVAL, 100).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {line_name}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_server_shared.hrl").
-include("base_line_def.hrl").

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------
%%% put(key, value)、get(key)在进程字典中存储和检索键值对
%%% 进程字典是一个进程独有的、存储键值对的数据结构
%% --------------------------------------------------------------------
%%	TODO:this proc not need again!!!!!!!
start_link({LineName, NamedProc})->
	?base_gen_server:start_link({local, LineName}, ?MODULE, {LineName, NamedProc}, []).

%% --------------------------------------------------------------------
%%% Description: get the maps that belongs to this line server.
%%% AllMaps: the map data of all line server.
%%% LineName: this line server' name.
%%% Return: maps that belongs to this line server.
%% --------------------------------------------------------------------
%% @spec create_map(atom(), atom()) -> list().
get_map(AllMaps, LineName, {SNode,Host,MapNode}) ->
	get_map2(AllMaps, LineName, {SNode,Host,MapNode}).

%% --------------------------------------------------------------------
%% Description: regist information into LineServer
%% Args: regist infromation
%% --------------------------------------------------------------------
%% @spec do_regist(atom(), atom()) -> void().
do_regist(regist_map_processor, Args, State) ->
	{NodeName, LineId, MapId, MapName} = Args,
	Key = make_map_id(LineId, MapId),
	%% {key, nodename, mapname, lineid, mapid, rolecount}
	%base_logger_util:info_msg("regist_map_processor ~p ~n ",[Args]),
	?base_ets:insert(?MAP_PROC_DB, {Key, NodeName, MapName, LineId, MapId}),
	ok.

do_regist(RegistType, Args) ->
	%% unkonwn format
	{LineId, MapId} = RegistType,
	Id = make_map_id(LineId, MapId),
	base_logger_util:info_msg("unknown regist type: ~s~n", [Id]).

unregist_by_node(NodeName)->
	AllNodeMaps = ?base_ets:match(?MAP_PROC_DB, {'$1', NodeName, '_', '_', '_'}),
	lists:foreach(fun(Key)-> ?base_ets:delete(?MAP_PROC_DB, Key) end, lists:append(AllNodeMaps)).
	

%% Description: get the map processor name by LineId and MapId
lookup_map_name(LineId, MapId) ->
	Key = make_map_id(LineId, MapId),
	case ?base_ets:lookup(?MAP_PROC_DB, Key) of
		[{_, NodeName, ProcName, _, _}]->
			{ok, {NodeName, ProcName}};
		[] ->
			{error}
	end.

%% Description: get the rold count by mapid
%% return: [{LineId, Count}, {LineId, Count}]
%% DB field: {key, nodename, mapname, lineid, mapid, rolecount}
%%		   {'_', '_',	  '_',	 '_',	mapid, '$1'}).
get_role_count_by_map(MapId) ->
	case ?base_ets:match(?MAP_PROC_DB, {'_', '_', '_', '$1', MapId}) of
		[]->
			base_logger_util:info_msg("get_role_count_by_map error ! MapId ~p ~n",[MapId]),
			[];
		LineIdList->
			OrgList = lists:map(fun(LineId)-> {LineId,0} end, lists:append(LineIdList)),
			MaxCount = base_env_ets:get2(line_switch,open_count,200),
			DynLineList =  lists:filter(fun(Lid)-> lists:keymember(Lid, 1, OrgList) end, get(dynamic_lines)),
			Fun = 
				fun(RolePos,{CountTmp,LineInfo})->
						LineId = role_pos_db:get_role_lineid(RolePos),
						LineRoleCount = 
						if
							LineId>0->
								case lists:keyfind(LineId, 1, LineInfo) of
									false-> LineInfo;%%[{LineId,1}|LineInfo]; no this map
									{_LineId,Count}->
										lists:keyreplace(LineId, 1, LineInfo, {LineId,Count+1})
								end;
							true->
								LineInfo
						end,
						{CountTmp+1,LineRoleCount}
				end,
			{AllOnlineNum,LineRoleCountOri} = role_pos_db:foldl(Fun ,{0,OrgList}),
			RoleCountWithoutIdelDynLine =  lists:filter(fun({LineId,Count})->
									   		(Count =/= 0) or (not lists:member(LineId, DynLineList))
							   		end, LineRoleCountOri),
			LiveLine = max(length(RoleCountWithoutIdelDynLine),1),
			LonelyMaps = base_map_info_db:get_lonely_maps(),
			case ( (AllOnlineNum/LiveLine) >=MaxCount) and (not lists:member(MapId,LonelyMaps)) of
				true->
		   			{_,RoleCountWithNewLine} = lists:foldl(fun(DyLineId,Acc0)->
														case Acc0 of
															{true,_}-> Acc0;
															{false,RoleCountX}->
																case lists:keyfind(DyLineId, 1, RoleCountWithoutIdelDynLine) of
																		false->{true, RoleCountX ++ [{DyLineId,0}]};
																	_-> Acc0
																end
														end
												end, {false,RoleCountWithoutIdelDynLine}, DynLineList),
			   		RoleCountWithNewLine;
		   		_->
			   		RoleCountWithoutIdelDynLine
			end
	end.

%% Description: 
get_role_count_by_line_map(MapId, LineId) ->
	RoleInfo = role_pos_db:get_role_info_by_map_line(MapId, LineId),
	erlang:length(RoleInfo).

%%
%%get role num in the map (all line)
%%return [{MapId,RoleNum},.....]
%%
get_role_num_by_mapId() ->
	Fun = 
		fun(RolePos,TempList)->
				MapId = role_pos_db:get_role_mapid(RolePos),
				case lists:keyfind(MapId,1,TempList) of
					false-> [{MapId,1}|TempList];
					{_MapId,Count}->
						lists:keyreplace(MapId, 1, TempList, {MapId,Count+1})
				end
		end,
	role_pos_db:foldl(Fun ,[]).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init({LineName, NamedProc}) ->
	base_logger_util:info_msg("~p:~p({LineName:~p, NamedProc:~p})~n",[?MODULE,?FUNCTION_NAME,LineName,NamedProc]),
	base_line_manager_server:regist_to_manager(NamedProc,LineName),
	{ok, #state{line_name = LineName}}.

?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
get_map2([H|T], LineName, {SNode,Host,MapNode}) ->
	{Line, Maps} = H,
	case Line =:= LineName of
		true ->
			Fun = fun(MapRec) ->
						{_, Node} = MapRec,
						Node =:= SNode				  
				  end,
			Filt = lists:filter(Fun, Maps),
			MakeNode = lists:map(fun({MapId,X})-> 
										%% NodeStr = atom_to_list(X) ++"@" ++ Host,
										%% Node = list_to_atom(NodeStr),
										%% {MapId,Node}
										{MapId,MapNode}
								 end, Filt),
			{true, MakeNode};
		false ->
			get_map2(T, LineName, {SNode,Host,MapNode})
	end;
%% This line is empyt line, does not contain map
get_map2([], _LineName, _MapNodeName) ->
	{false}.

make_map_id(LineId, MapId) ->
	integer_to_list(LineId) ++"_"++ integer_to_list(MapId).
