%% Description: TODO: Add description to map_app
-module(base_map_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("reloader.hrl").

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1,
	 start/0
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
	case util:get_argument('-line') of
		[]->  base_logger_util:msg("Missing --line argument input the nodename");
		[CenterNode|_]->
			filelib:ensure_dir("../log/"),
			FileName = "../log/"++atom_to_list(base_node_util:get_node_sname(node())) ++ "_node.log", 
			error_logger:logfile({open, FileName}),
			% ?RELOADER_RUN,
			base_ping_util:wait_all_nodes_connect(),
			base_db_tools:wait_line_db(),

			base_global_util:wait_global_proc_register(),
			base_timer_server:start_at_app(),
			base_db_sup:start_db_dmp_server(),
			%%wait all db table
			base_logger_util:msg("wait_for_all_db_tables ing ~n"),
			% 初始化当前节点的ets配置列表,并等待db节点数据库就绪
			base_application_server:wait_ets_init(),
			base_logger_util:msg("wait_for_all_db_tables end ~n"),
			% role_pos_db:unreg_role_pos_to_mnesia_by_node(node()),
			% role_app:start(),
			base_lines_manager_server:wait_lines_manager_loop(),

			%%load map
			start_map_sup(),
			start_map_manager_sup(),

			% case node_util:check_node_allowable(battle_ground_manager, node()) of
			% 	true->
			% 		start_battle_ground_sup(),
			% 		start_battle_ground_manager_sup();
			% 	false->
			% 		start_battle_ground_sup()
			% end,

			{ok, self()}
	end.

start()->
	base_application_server:start(?MODULE).
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

start_map_sup()->
	case base_map_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

start_map_manager_sup()->
	case base_map_manager_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

% start_map_db_sup()->
% 	case base_map_db_sup:start_link() of
% 		{ok,Pid} ->
% 			{ok,Pid};
% 		Error ->
% 			Error
% 	end.