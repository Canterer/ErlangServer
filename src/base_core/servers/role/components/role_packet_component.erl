%% Description: TODO: Add description to role_packet_component
-module(role_packet_component).
-export([
	handle_event/4,
	new_player_notify/1
]).

-include("base_component_shared.hrl").

new_player_notify(OffLine)->
	case OffLine of
		{0,0,0}->				%%发送新手通知
			NewCommerMsg = role_packet:encode_is_jackaroo_s2c(),
			% send_data_to_gate(NewCommerMsg);
			apply_component(send_to_gate_component,send_data_to_gate,[NewCommerMsg]);
		_->
			nothing
	end.

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.
