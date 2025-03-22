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
	todo.

%%
%%get role num in the map (all line)
%%return [{MapId,RoleNum},.....]
%%
get_role_num_by_mapId() ->
	todo.

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
