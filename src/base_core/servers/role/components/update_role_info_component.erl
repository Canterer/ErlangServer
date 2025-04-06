%% Description: TODO: Add description to update_role_info_component
-module(update_role_info_component).

-export([
	handle_event/4,
	update_role_info/0,
	update_role_info/2
]).

-include("base_component_shared.hrl").

update_role_info()->
	update_role_info(get(roleid),get(creature_info)).	
update_role_info(RoleId, RoleInfo) ->
	base_role_manager:regist_role_info(RoleId, RoleInfo).

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.