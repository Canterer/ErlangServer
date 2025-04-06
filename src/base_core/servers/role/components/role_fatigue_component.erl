%% Description: TODO: Add description to role_fatigue_component
-module(role_fatigue_component).
-export([
	handle_event/4,
	init/0,
	on_playeronline/0
]).

-include("base_component_shared.hrl").

init()->
	case get(is_adult) of
		false-> fatigue:init();
		true-> ignor
	end.

on_playeronline()->
	case get(is_adult) of
		false-> fatigue:on_playeronline(get(account_id));
		true->ignor
	end.

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.