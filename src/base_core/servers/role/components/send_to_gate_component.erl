%% Description: TODO: Add description to send_to_gate_component
-module(send_to_gate_component).
-export([
	handle_event/4,
	send_data_to_gate/1,
	send_data_to_gate/2
]).

-include("base_component_shared.hrl").
-include("data_struct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 发送数据给角色对应的网关进程
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_data_to_gate(Message) ->
	%%@@base_logger_util:info_msg("modules.role.base_role_op:send_data_to_gate/1 Message~p~n",[Message]),
	GateProc = get_proc_from_gs_system_gateinfo(get(gate_info)),
	base_tcp_client_statem:send_data(GateProc, Message).
send_data_to_gate(DelayTime, Message) ->
	%%@@base_logger_util:info_msg("modules.role.base_role_op:send_data_to_gate/2 Message~p~n",[Message]),
	GatePid = get_pid_from_gs_system_gateinfo(get(gate_info)),
	base_tcp_client_statem:send_data_after(GatePid,Message,DelayTime).


handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.