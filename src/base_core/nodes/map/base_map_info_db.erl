-module(base_map_info_db).
-include("mnesia_table_def.hrl").
-define(MAP_INFO_ETS,map_info_ets).
-compile(export_all).

-export([start/0,create_mnesia_table/1,create_mnesia_split_table/2,delete_role_from_db/1,tables_info/0]).
-export([init_ets/0,create_ets/0]).
-behaviour(db_operater_behaviour).
-behaviour(ets_operater_behaviour).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()->
	db_operater_behaviour:start_module(?MODULE,[]).

create_mnesia_table(disc)->
	base_db_tools:create_table_disc(map_info, record_info(fields,map_info), [], set).

create_mnesia_split_table(_,_)->
	nothing.

tables_info()->
	[{map_info,proto}].

delete_role_from_db(_)->
	nothing.

create_ets()->	
	ets_operater_behaviour:new(?MAP_INFO_ETS, [set,named_table]).

init_ets()->
	% 从数据库读取 
	db_operater_behaviour:init_ets(map_info, ?MAP_INFO_ETS, #map_info.mapid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	base_logger_util:msg("~p:line:~p~n",[?MODULE,?LINE]),
	ets:foldl(fun({_,MapInfo},Acc)->
						base_logger_util:msg("~p:line:~p MapInfo:~p Acc:~p LineTag:~p~n",[?MODULE,?LINE,MapInfo,Acc,LineTag]),
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
