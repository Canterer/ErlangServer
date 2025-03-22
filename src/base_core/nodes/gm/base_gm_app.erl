%% Description: TODO: Add description to gate_network
-module(base_gm_app).

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
-export([
	start/0,
	gm_listener_started/2,
	gm_listener_stopped/2,
	start_client/2
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
	%gs_prof:support(),
	case base_node_util:get_argument('-line') of
		[]->  base_logger_util:info_msg("Missing --line argument input the nodename");
		[CenterNode|_]->
			% ?RELOADER_RUN,	
			base_ping_util:wait_all_nodes_connect(),
			% case server_travels_util:is_share_server() of
			% 	false->
			% 		travel_deamon_sup:start_link();
			% 	true->
			% 		nothing
			% end,
			
			% application启用后统一操作
			%% rpc获取timer节点时间,记录相对时间偏差
			base_timer_server:start_at_app(),

			% statistics_sup:start_link(),
			% boot_gm_msgwrite_sup(),
			% boot_gm_msgwrite_mysql_sup(),
			% boot_mysql_sup(),
			start_boot_client_sup(),
			start_boot_listener_sup(),
			{ok, self()}
	end.

start()->
	base_application_server:start(?MODULE).

start_boot_listener_sup() ->	
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	SName = base_node_util:get_node_sname(node()),
	?ZS_LOG("node:~p SName:~p",[node(),SName]),
	Port = base_env_ets:get2(gmport, SName, 0),
	?ZS_LOG("Port:~p",[Port]),
	case Port of
		0-> base_logger_util:info_msg("start gate error ,can not find listen port~n"),error;
		Port->
			AcceptorCount = base_env_ets:get2(gm,acceptor_count,1),
			OnStartup = {?MODULE,gm_listener_started,[]},
			OnShutdown = {?MODULE,gm_listener_stopped,[]},
			AcceptCallback={?MODULE,start_client,[]},
			case base_gm_listener_sup:start_link(Port ,OnStartup, OnShutdown, AcceptCallback,AcceptorCount) of
				{ok, Pid} ->
					{ok, Pid};
				Error ->
					Error
			end
	end.

start_boot_client_sup() ->
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	Client_Plugin = socket_callback:get_client_mod(),
	base_gm_client_sup:start_link(Client_Plugin).

boot_gm_msgwrite_sup()->
	gm_msgwrite_sup:start_link().

boot_gm_msgwrite_mysql_sup()->
	gm_msgwrite_mysql_sup:start_link().

boot_mysql_sup()->
	mysql_sup:start_link(),
	mysql_sup:start_mysql().


%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
	ok.
%% --------------------------------------------------------------------
%% Func: gm_listen_start/2
%% Returns: any
%% --------------------------------------------------------------------
gm_listener_started(IPAddress, Port) ->
	base_logger_util:info_msg("GM Started at ~p : ~p\n", [IPAddress, Port]).


gm_listener_stopped(IPAddress, Port) ->
	base_logger_util:info_msg("GM Stopped at ~p : ~p\n", [IPAddress, Port]).

start_client(Sock,Pid)->
	base_logger_util:info_msg("start one gm client process pid = ~p sock = ~p\n",[Pid,Sock]).

%% ====================================================================
%% Internal functions
%% ====================================================================
