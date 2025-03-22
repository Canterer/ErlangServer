-module(base_role_sup).
-include("base_define_min.hrl").
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ROLES_DB,local_roles_datatbase).
-define(PETS_DB,local_pets_datatbase).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_role/2,stop_role/2,start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	init/1
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->	
	supervisor:start_link({local,?SERVER}, ?MODULE, []).

start_role(RoleDB,RoleId) ->
	base_logger_util:info_msg("role_sup:start_role:~p~n", [RoleId]),
	ChildSpec= {RoleId,{base_role_processor,start_link,[RoleDB,RoleId]},
			temporary, 2000, worker,[base_role_processor]},
	supervisor:start_child(?SERVER, ChildSpec).

stop_role(RoleSupNode, RoleId)->
	supervisor:terminate_child({?SERVER,RoleSupNode} , RoleId),
	supervisor:delete_child({?SERVER, RoleSupNode}, RoleId).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%		  ignore						  |
%%		  {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	%% {RoleId,RoleProc,GateNode,GateProc,MapProc,Coord} 
	?base_ets:new(?ROLES_DB, [set,public,named_table]),
	?base_ets:new(?PETS_DB, [set,public,named_table]),
	
	ManagerSpec ={base_role_manager,{base_role_manager,start_link,[?ROLES_DB]},permanent,2000,worker,[base_role_manager]}, 
	{ok,{{one_for_one,10,10}, [ManagerSpec]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
