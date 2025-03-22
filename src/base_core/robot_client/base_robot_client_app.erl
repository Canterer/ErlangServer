%% Description: TODO: Add description to robot_client_app
-module(base_robot_client_app).

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
	stop/1
]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([start/3]).

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
start(_Type, Arg) ->
	base_logger_util:info_msg("_Type:~p Arg:~p~n", [_Type, Arg]),
	base_logger_util:info_msg("~p:~p:line:~p~n",[?MODULE,?FUNCTION_NAME,?LINE]),
	{ok,Id} = application:get_env(?MODULE, id),
	{ok,Client_config} = application:get_env(?MODULE, client_config),
	base_logger_util:info_msg("Id:~p Client_config:~p~n", [Id, Client_config]),
	base_robot_client_sup:start_link([Id, Client_config]).

start(Id, Client_config, _)->
	base_logger_util:info_msg("~p:~p:line:~p~n",[?MODULE,?FUNCTION_NAME,?LINE]),
	base_logger_util:info_msg("Id:~p Client_config:~p~n", [Id, Client_config]),
	application:set_env(?MODULE, id, Id),
	application:set_env(?MODULE, client_config, Client_config),
	application:start(?MODULE).
	% base_application_server:start(?MODULE).


%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

