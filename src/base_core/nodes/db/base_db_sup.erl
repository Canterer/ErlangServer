-module(base_db_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	init/1,
	start_master/0,
	start_db_line_master/0,
	start_db_dmp_server/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

% 用于管理disc数据
start_master()->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	supervisor:start_link({local,?SERVER}, ?MODULE, [master]).

% 用于管理ram数据
start_db_line_master()->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	supervisor:start_link({local,?SERVER}, ?MODULE, [db_line_master]).

% data modify processor of DAL  数据修改进程  用于DAL数据库访问层
start_db_dmp_server()->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	supervisor:start_link({local,?SERVER}, ?MODULE, [db_dmp_server]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%		  ignore						  |
%%		  {error, Reason}
%% --------------------------------------------------------------------
init([DbType]) ->
	case DbType of
		master->
			AChildList = [{base_db_master_server,{base_db_master_server,start_link,[]}, permanent,2000,worker,[base_db_master_server]}];
		db_dmp_server->
			 AChildList = [{base_db_dmp_server,{base_db_dmp_server,start_link,[]}, permanent,2000,worker,[base_db_dmp_server]}];
		db_line_master->
			AChildList = [{base_db_line_master_server,{base_db_line_master_server,start_link,[]}, permanent,2000,worker,[base_db_line_master_server]}]						  
	end,
	{ok,{{one_for_all,10,10}, AChildList}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

