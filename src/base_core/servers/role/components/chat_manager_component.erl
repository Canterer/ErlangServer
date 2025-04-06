%% Description: TODO: Add description to chat_manager_component
-module(chat_manager_component).
-export([
	handle_event/4,
	start_chat_role/2,
	start_chat_role/3,
	stop_chat_role/3
]).

-include("base_component_shared.hrl").

start_chat_role(GS_system_role_info,GS_system_gate_info)->
	case chat_manager:start_chat_role(GS_system_role_info, GS_system_gate_info) of
		error->
			base_logger_util:info_msg("chat_manager:start_chat_role error ~n"),
			self() ! {tcp_closed, 0},
			{0,0};
		Re->
			Re
	end.
start_chat_role(Chat_node,GS_system_role_info,GS_system_gate_info)->
	case chat_manager:start_chat_role(Chat_node, GS_system_role_info, GS_system_gate_info) of
		error->
			base_logger_util:info_msg("chat_manager:start_chat_role error ~n"),
			self() ! {tcp_closed, 0},
			{0,0};
		Re->
			Re
	end.

stop_chat_role(Chat_node,Chat_proc,RoleId)->
	chat_manager:stop_chat_role(Chat_node,Chat_proc,RoleId).

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.