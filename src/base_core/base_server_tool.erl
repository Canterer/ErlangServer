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
	% filelib:ensure_dir("../log/"),
	% error_logger:logfile({open, "../log/run.log"}),
	
	% 开启base_application服务 
	base_application_server:force_start(),

	Node = base_node_util:get_node_sname(node()),
	base_logger_util:msg("cur node name:~p~n",[Node]),


	% 不同节点 check各自的运行	
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
	case base_node_util:check_node_allowable(line, node()) of
		true-> base_line_app:start();
		_-> ignor
	end.

check_timer_run()->
	case base_node_util:check_node_allowable(timer, node()) of
		true-> base_timer_app:start();
		_-> ignor
	end.

check_db_run()->
	case base_node_util:check_node_allowable(db, node()) of
		true-> base_db_app:start();
		_-> ignor
	end.

check_map_run()->
	case base_node_util:check_node_allowable(map, node()) of
		true-> base_map_app:start();
		_-> ignor
	end.
%%------------------------------------------------------------------------------------------
%% for tools 
%%------------------------------------------------------------------------------------------

%%------------------------------------------------------------------------------------------
%% for tools 
%%------------------------------------------------------------------------------------------
