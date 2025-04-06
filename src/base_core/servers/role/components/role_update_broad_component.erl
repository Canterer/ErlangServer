%% Description: TODO: Add description to role_update_broad_component
-module(role_update_broad_component).
-export([
	handle_event/4,
	only_self_update/1,
	self_update_and_broad/1
]).

-include("base_component_shared.hrl").
-include("data_struct.hrl").
-include("map_info_struct.hrl").
-include("common_define.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%									属性广播
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
only_self_update([])->
	nothing;
only_self_update(UpdateAttr)->
	UpdateObj = object_update:make_update_attr(?UPDATETYPE_SELF,get(roleid),UpdateAttr),
	GateProc = get_proc_from_gs_system_gateinfo(get(gate_info)),
	base_tcp_client_statem:object_update_update(GateProc,UpdateObj).
	
self_update_and_broad([])->
	nothing;
self_update_and_broad(UpdateAttr)->
	only_self_update(UpdateAttr),
	UpdateObj = object_update:make_update_attr(?UPDATETYPE_ROLE,get(roleid),UpdateAttr),
	creature_op:direct_broadcast_to_aoi_gate({object_update_update,UpdateObj}).


handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.