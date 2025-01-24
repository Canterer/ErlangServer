%% Description: TODO: Add description to line_app
-module(base_line_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
% -include("reloader.hrl").

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 start/0	 
	]).

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
	filelib:ensure_dir("../log/"),
	error_logger:logfile({open, "../log/line_node.log"}),
	% ?RELOADER_RUN,
	% 等待配置中的pre_connect_nodes节点可连接
	base_ping_util:wait_all_nodes_connect(),
	% 通过base_db_sup监控树开启base_db_line_master服务
	base_db_sup:start_db_line_master(),% line_master用于管理ram数据
	

	% application启用后统一操作
	%% rpc获取timer节点时间,记录相对时间偏差
	base_timer_server:start_at_app(),
	% 从表中读取全局进程名
	base_global_proc_util:wait_global_proc_register(),
	% 初始化当前节点的ets配置列表,并等待db节点数据库就绪
	base_application_server:wait_ets_init(),
	
	start_lines_manager_sup(),
	start_line_processor_sup(),
	{ok, self()}.
	
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

start_lines_manager_sup() ->
	case base_line_manager_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

start_line_processor_sup() ->
	case base_line_processor_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.