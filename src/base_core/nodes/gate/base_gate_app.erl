%% Description: TODO: Add description to gate_network
-module(base_gate_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("reloader.hrl").
-include("base_define_shared.hrl").

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	start/2,
	stop/1
]).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0,tcp_listener_started/2,tcp_listener_stopped/2,start_client/2]).

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
	%gs_prof:support(),
	case base_node_util:get_argument('-line') of
		[]->  base_logger_util:info_msg("Missing --line argument input the nodename");
		[CenterNode|_]->
			% ?RELOADER_RUN,	
			base_ping_util:wait_all_nodes_connect(),
			base_db_tools:wait_line_db(),
			% case server_travels_util:is_share_server() of
			% 	false->
			% 		travel_deamon_sup:start_link();
			% 	true->
			% 		nothing
			% end,
			
			% application启用后统一操作
			%% rpc获取timer节点时间,记录相对时间偏差
			base_timer_server:start_at_app(),
			% 从表中读取全局进程名
			base_global_proc_util:wait_global_proc_register(),
			% 初始化当前节点的ets配置列表,并等待db节点数据库就绪
			base_application_server:wait_ets_init(),

			% statistics_sup:start_link(),
			start_base_tcp_client_sup(),
			start_base_tcp_listener_sup(),
			{ok, self()}
	end.

start()->
	base_application_server:start(?MODULE).

start_base_tcp_listener_sup() ->	
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	SName = base_node_util:get_node_sname(node()),
	?ZS_LOG("node:~p SName:~p",[node(),SName]),
	Port = base_env_ets:get2(gateport, SName, 0),
	?ZS_LOG("Port:~p",[Port]),
	case Port of
		0-> base_logger_util:info_msg("start gate error ,can not find listen port~n"),error;
		Port->
			AcceptorCount = base_env_ets:get2(gate,acceptor_count,1),
			OnStartup = {?MODULE,tcp_listener_started,[]},
			OnShutdown = {?MODULE,tcp_listener_stopped,[]},
			AcceptCallback={?MODULE,start_client,[]},
			case base_tcp_listener_sup:start_link(Port ,OnStartup, OnShutdown, AcceptCallback,AcceptorCount) of
				{ok, Pid} ->
					{ok, Pid};
				Error ->
					Error
			end
	end.

start_base_tcp_client_sup() ->
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	Client_Plugin = socket_callback:get_client_mod(),
	base_tcp_client_sup:start_link(Client_Plugin).


%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
	ok.
%% --------------------------------------------------------------------
%% Func: tcp_listen_start/2
%% Returns: any
%% --------------------------------------------------------------------
tcp_listener_started(IPAddress, Port) ->
	base_logger_util:info_msg("Game Gate Started at ~p : ~p\n", [IPAddress, Port]).


tcp_listener_stopped(IPAddress, Port) ->
	base_logger_util:info_msg("Game Gate Stopped at ~p : ~p\n", [IPAddress, Port]).

start_client(Sock,Pid)->
	base_logger_util:info_msg("start one client process pid = ~p sock = ~p\n",[Pid,Sock]).

%% ====================================================================
%% Internal functions
%% ====================================================================
