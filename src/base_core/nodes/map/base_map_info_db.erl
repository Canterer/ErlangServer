-module(base_map_info_db).

-export([
	get_map_info/1,
	get_mapid/1,
	get_is_instance/1,
	get_map_tag/1,
	get_restrict_items/1,
	get_map_name/1,
	get_script/1,
	get_can_flyshoes/1,
	get_linetag/1,
	get_serverdataname/1,
	get_pvptag/1,
	get_maps_bylinetag/1,
	get_all_maps_and_serverdata/0,
	get_lonely_maps/0
]).

-define(MAP_INFO_ETS,map_info_ets).

-include("mnesia_table_def.hrl").
-include("base_define_shared.hrl").
%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
% -include("base_ets_operater_shared.hrl").
% -include("base_db_operater_shared.hrl").
-define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
%% --------------------------------------------------------------------
%%% behaviour functions begine
%% --------------------------------------------------------------------
do_create_ets()->	
	ets_operater_behaviour:new(?MAP_INFO_ETS, [set,named_table]).

do_init_ets()->
	% 从数据库读取 
	db_operater_behaviour:init_ets(map_info, ?MAP_INFO_ETS, #map_info.mapid).

do_start()->
	db_operater_behaviour:start_module(?MODULE,[]).

do_create_mnesia_table(disc)->
	base_db_tools:create_table_disc(map_info, record_info(fields,map_info), [], set).

do_create_mnesia_split_table(_,_)->
	nothing.

do_tables_info()->
	[{map_info,proto}].

do_delete_role_from_db(_)->
	nothing.

%% --------------------------------------------------------------------
%%% behaviour functions end
%% --------------------------------------------------------------------

get_map_info(MapId)->
	case ets:lookup(?MAP_INFO_ETS,MapId) of
		[]-> [];
		[{_,MapInfo}]-> MapInfo
	end.

get_mapid(MapInfo)->
	element(#map_info.mapid,MapInfo).

get_is_instance(MapInfo)->
	element(#map_info.is_instance,MapInfo).

get_map_tag(MapInfo)->
	element(#map_info.map_tag,MapInfo).

get_restrict_items(MapInfo)->
	element(#map_info.restrict_items,MapInfo).

get_map_name(MapInfo)->
	element(#map_info.map_name,MapInfo).

get_script(MapInfo)->
	element(#map_info.script,MapInfo).

get_can_flyshoes(MapInfo)->
	element(#map_info.can_flyshoes,MapInfo).

get_linetag(MapInfo)->
	element(#map_info.linetag,MapInfo).

get_serverdataname(MapInfo)->
	element(#map_info.serverdataname,MapInfo).

get_pvptag(MapInfo)->
	element(#map_info.pvptag,MapInfo).

get_maps_bylinetag(LineTag)->
	?ZS_LOG(),
	ets:foldl(fun({_,MapInfo},Acc)->
						?ZS_LOG("MapInfo:~p Acc:~p LineTag:~p",[MapInfo,Acc,LineTag]),
						case get_linetag(MapInfo) of
						  []->
							  [get_mapid(MapInfo)|Acc];
						  LineTags->
							  case lists:member(LineTag, LineTags) of
								  true->
							  		[get_mapid(MapInfo)|Acc];
								  _->
									  Acc
							  end
						end
			  end, [], ?MAP_INFO_ETS).

get_all_maps_and_serverdata()->		
	ets:foldl(fun({_,MapInfo},Acc)->				 
					[{get_mapid(MapInfo),get_serverdataname(MapInfo)}|Acc]
			  end, [], ?MAP_INFO_ETS).

get_lonely_maps()->
	ets:foldl(fun({_,MapInfo},Acc)->
						case length(get_linetag(MapInfo)) =:= 1 of
							true->
							  	[get_mapid(MapInfo)|Acc];
							_->
								Acc
						end
			  end, [], ?MAP_INFO_ETS).
