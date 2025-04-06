-module(instance_proto_db).

-export([
	get_info/1,
	get_protoid/1,
	get_type/1,
	get_create_leadertag/1,
	get_create_item/1,
	get_level/1,
	get_membernum/1,
	get_dateline/1,
	get_quests/1,
	get_item_need/1,
	get_can_direct_exit/1,
	get_datetimes/1,
	get_restrict_items/1,
	get_level_mapid/1,
	get_duration_time/1,
	get_nextproto/1
]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
-define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
-include("mnesia_table_def.hrl").
-define(INSTANCE_PROTO_NAME,ets_instance_proto).
%% --------------------------------------------------------------------
%%% behaviour functions begine
%% --------------------------------------------------------------------
?create_ets()->
	?base_ets:new(?INSTANCE_PROTO_NAME, [set,named_table]).

?init_ets()->
	db_operater_behaviour:init_ets(instance_proto, ?INSTANCE_PROTO_NAME,#instance_proto.protoid).

?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	base_db_tools:create_table_disc(instance_proto, record_info(fields,instance_proto), [], set).	

?create_mnesia_split_table(_,_)->
	nothing.

?delete_role_from_db(_)->
	nothing.

?tables_info()->
	[{instance_proto,proto}].




get_info(ProtoId)->
	case ?base_ets:lookup(?INSTANCE_PROTO_NAME,ProtoId) of
		[]->[];
		[{ProtoId,Term}]-> Term
	end.

get_protoid(ProtoInfo)->
	erlang:element(#instance_proto.protoid, ProtoInfo).

get_type(ProtoInfo)->
	erlang:element(#instance_proto.type, ProtoInfo).

get_create_leadertag(ProtoInfo)->
	erlang:element(#instance_proto.create_leadertag, ProtoInfo).

get_create_item(ProtoInfo)->
	erlang:element(#instance_proto.create_item, ProtoInfo).

get_level(ProtoInfo)->
	erlang:element(#instance_proto.level, ProtoInfo).

get_membernum(ProtoInfo)->
	erlang:element(#instance_proto.membernum, ProtoInfo).

get_dateline(ProtoInfo)->
	erlang:element(#instance_proto.dateline, ProtoInfo).

get_quests(ProtoInfo)->
	erlang:element(#instance_proto.quests, ProtoInfo).

get_item_need(ProtoInfo)->
	erlang:element(#instance_proto.item_need, ProtoInfo).

get_can_direct_exit(ProtoInfo)->
	erlang:element(#instance_proto.can_direct_exit, ProtoInfo).

get_datetimes(ProtoInfo)->
	erlang:element(#instance_proto.datetimes, ProtoInfo).

get_restrict_items(ProtoInfo)->
	erlang:element(#instance_proto.restrict_items, ProtoInfo).

get_level_mapid(ProtoInfo)->
	erlang:element(#instance_proto.level_mapid, ProtoInfo).

get_duration_time(ProtoInfo)->
	erlang:element(#instance_proto.duration_time, ProtoInfo).

get_nextproto(ProtoInfo)->
	erlang:element(#instance_proto.nextproto, ProtoInfo).
