%% Description: TODO: Add description to role_quick_bar_component
-module(role_quick_bar_component).
-export([
	handle_event/4,
	send_display_hotbar/1,
	update_hotbar/3
]).

-include("base_component_shared.hrl").
-include("role_struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 返回快捷栏的内容
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_display_hotbar(RoleInfo) ->
	RoleId = get_id_from_roleinfo(RoleInfo),
	EntryList = skill_db:get_quick_bar(RoleId),
	Message = role_packet:encode_display_hotbar_s2c(EntryList),
	% send_data_to_gate(Message).
	apply_component(send_to_gate_component,send_data_to_gate,[Message]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 更新快捷栏的内容
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_hotbar(SlotId, ClassId, EntryId) ->
	skill_db:update_quick_bar(get(roleid), SlotId, ClassId, EntryId).

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.