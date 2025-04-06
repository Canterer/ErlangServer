%% Description: TODO: Add description to map_op_component
-module(map_op_component).
-export([
	handle_event/4,
	init_npc_info/1,
	copy_init/1
]).

-include("base_component_shared.hrl").
-include("data_struct.hrl").
-include("map_info_struct.hrl").

init_npc_info(GS_MapInfo)->
	%% 设置地图信息
	case get_proc_from_gs_system_mapinfo(GS_MapInfo) of
		undefined->
			% MapProc = undefined, 
			put(npcinfo_db,undefined),
			undefined;	
		MapProc->
			NpcInfoDB = npc_op:make_npcinfo_db_name(MapProc),
			put(npcinfo_db,NpcInfoDB),
			% ok
			MapProc
	end.

copy_init(MapInfo)->
	MapProc = get_proc_from_mapinfo(MapInfo),
	NpcInfoDB = npc_op:make_npcinfo_db_name(MapProc),
	put(npcinfo_db,NpcInfoDB).

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.