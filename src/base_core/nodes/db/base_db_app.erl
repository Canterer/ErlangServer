-module(base_db_app).

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
	start/0,
	import/0
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

start()->
	base_application_server:start(?MODULE).

import()->
 	base_db_data_gen_util:import_config("game"),
	mnesia:stop(),
	erlang:halt().


%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}		|
%%		  {ok, Pid, State} |
%%		  {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
	do_start().

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

do_start()->
	% ?RELOADER_RUN,
	% 仅等待timer节点
	base_ping_util:wait_node_connect(timer),

	% application启用后统一操作
	% 从表中读取全局进程名
	base_global_proc_util:wait_global_proc_register(),
	% rpc获取timer节点时间,记录相对时间偏差
	base_timer_server:start_at_app(),

	case base_db_sup:start_master() of
		{ok, _Pid} ->
			base_db_tools:wait_for_tables_loop(local,1000,mnesia:system_info(tables)),
			base_logger_util:msg("---------------------------------------------------------~n"),
			base_logger_util:msg("2)To generate data input: data_gen:import_config(\"game\").~n"),
			base_logger_util:msg("3)To backup   data input: db_backup:backup(YourFileName).~n"),
			base_logger_util:msg("4)To recovery data input: db_backup:recovery(YourFileName).~n"),
			base_logger_util:msg("5)To recovery data input: data_gen:backup_ext(YourFileName).~n"),
			base_logger_util:msg("6)To recovery data input: data_gen:recovery_ext(YourFileName).~n"),
			base_logger_util:msg("7)To recovery data input: combin_server:backup(YourFileName).~n"),
			base_logger_util:msg("8)To recovery data input: combin_server:recovery(YourFileName).~n"),
			base_logger_util:msg("---------------------------------------------------------~n");
		Error ->
			Error
	end,
	{ok, self()}.
