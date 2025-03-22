%% Description: TODO: Add description to base_robot_client_sup
-module(base_robot_client_sup).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/1
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_sup_shared.hrl").

%% ====================================================================
%% External functions
%% ====================================================================
start_link([Id, Client_config])->
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	base_logger_util:info_msg("Id:~p Client_config:~p~n", [Id, Client_config]),

	supervisor:start_link({local, ?SERVER},?MODULE, [Id, Client_config]).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([Id, Client_config]) ->
	base_logger_util:info_msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	base_logger_util:info_msg("Id:~p Client_config:~p~n", [Id, Client_config]),

	Worker = {base_robot_client_fsm,{base_robot_client_fsm,start,[Id, Client_config, false]},
					 	temporary, 2000,worker,[base_robot_client_fsm]},
	{ok,{{one_for_one, 5, 60}, [Worker]}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
