%% Description: TODO: Add description to base_server_control_sup
-module(base_server_control_sup).
-behaviour(supervisor).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/0]).
-export([init/1]).

%%
%% API Functions
%%
start_link() ->
		supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	Server_Control_Server = {base_server_control_server,{base_server_control_server,start_link,[]},
	       permanent,2000,worker,[base_server_control_server]},
	{ok,{{one_for_one, 10, 10}, [Server_Control_Server]}}.

%%
%% Local Functions
%%

