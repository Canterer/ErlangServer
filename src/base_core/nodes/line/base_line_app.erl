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
%% Returns: {ok, Pid}		|
%%		  {ok, Pid, State} |
%%		  {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
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
	
	% base_line_processor_sup 需要在 base_line_manager_sup 之前初始化
	start_line_processor_sup(),
	start_lines_manager_sup(),

	start_role_id_generator_sup(),
	start_item_id_generator_sup(),

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
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	case base_line_manager_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

start_line_processor_sup() ->
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	case base_line_processor_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

start_role_id_generator_sup() ->
	case base_role_id_generator_sup:start_link() of
		{ok,Pid}->
			{ok, Pid};
		Error ->
			Error
	end.

start_item_id_generator_sup() ->
	case base_item_id_generator_sup:start_link() of
		{ok,Pid}->
			{ok, Pid};
		Error ->
			Error
	end.