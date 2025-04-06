%% Description: TODO: Add description to role_package_component
-module(role_package_component).
-export([
	handle_event/4,
	send_items_onhands/0,
	send_items_on_storage/1
]).

-include("base_component_shared.hrl").

send_items_onhands()->
	%%发送
	HandsonItemid = package_op:get_items_id_on_hands(),
	HandsonItemInfo = lists:map(fun(Id)->items_op:get_item_info(Id)end,HandsonItemid),
	Message = role_packet:encode_init_onhands_item_s2c(HandsonItemInfo),
	% send_data_to_gate(Message).
	apply_component(send_to_gate_component,send_data_to_gate,[Message]).

send_items_on_storage(NpcId)->
	StorageIds = package_op:get_items_id_on_storage(),
	StorageInfo = lists:map(fun(Id)->items_op:get_item_info_storage(Id) end,StorageIds),
	Message = role_packet:encode_npc_storage_items_s2c(NpcId,StorageInfo),
	% send_data_to_gate(Message).
	apply_component(send_to_gate_component,send_data_to_gate,[Message]).

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.