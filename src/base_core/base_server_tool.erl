%% Description: 各个节点的统一入口
-module(base_server_tool).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([run/0]).

%%
%% API Functions
%%

run()->
	% 开启base_application服务 
	base_application_server:force_start(),
	% 不同节点 check各自的运行
	filelib:ensure_dir("../log/"),
	error_logger:logfile({open, "../log/run.log"}),

	Node = base_node_util:get_node_sname(node()),
	base_logger_util:msg("cur node name:~p~n",[Node]),
	%% line节点
	check_line_run(),
	%%timer节点
	check_timer_run(),
	% db节点
	check_db_run(),
	% map节点
	check_map_run().

%%
%% Local Functions
%%

check_line_run()->
	AllowableNodeList = base_node_util:get_allowable_nodes(line),
	base_logger_util:msg("line allowable_nodes:~p~n",[AllowableNodeList]),
	case base_node_util:check_node_allowable(AllowableNodeList, node()) of
		true-> base_line_app:start();
		_-> ignor
	end.

check_timer_run()->
	AllowableNodeList = base_node_util:get_allowable_nodes(timer),
	base_logger_util:msg("timer allowable_nodes:~p~n",[AllowableNodeList]),
	Node = base_node_util:get_node_sname(node()),
	case base_node_util:check_node_allowable(AllowableNodeList, node()) of
		true-> base_timer_app:start();
		_-> ignor
	end.

check_db_run()->
	AllowableNodeList = base_node_util:get_allowable_nodes(db),
	base_logger_util:msg("db allowable_nodes:~p~n",[AllowableNodeList]),
	Node = base_node_util:get_node_sname(node()),
	case base_node_util:check_node_allowable(AllowableNodeList, node()) of
		true-> base_db_app:start();
		_-> ignor
	end.

check_map_run()->
	AllowableNodeList = base_node_util:get_allowable_nodes(map),
	base_logger_util:msg("map allowable_nodes:~p~n",[AllowableNodeList]),
	Node = base_node_util:get_node_sname(node()),
	case base_node_util:check_node_allowable(AllowableNodeList, node()) of
		true-> base_map_app:start();
		_-> ignor
	end.
%%------------------------------------------------------------------------------------------
%% for tools 
%%------------------------------------------------------------------------------------------

%%------------------------------------------------------------------------------------------
%% for tools 
%%------------------------------------------------------------------------------------------
