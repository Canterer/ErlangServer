%% Description: TODO: Add description to env_ets
-module(env_ets).

%%
%% Include files
%%
-define(OPTION_ETS,option_value_ets).
-define(SERVER_NAME_ETS,option_servers_name).
-define(OPTION_FILE,"../option/game_server.option").
-define(GM_OPTION,"../option/gm.option").
-define(SERVER_START_TIME,"../option/server_start_time.option").
-define(SERVER_NAME_FILE,"../option/server_name.option").
% -define(NODE_OPTION_FILE,"../option/node.option").

%%
%% Exported Functions
%%

-export([init/0,fresh/0]).

%%
%% API Functions
%%

%%
%% Local Functions
%%

init()->
	base_env_ets:init(),
	fresh().

fresh()->
	base_env_ets:reset(),
	% base_env_ets:read_from_file(?NODE_OPTION_FILE,?OPTION_ETS),
	base_env_ets:read_from_file(?OPTION_FILE,?OPTION_ETS),
	base_env_ets:read_from_file(?GM_OPTION,?OPTION_ETS),
	base_env_ets:read_from_file(?SERVER_START_TIME,?OPTION_ETS),
	base_env_ets:read_from_file(?SERVER_NAME_FILE,?SERVER_NAME_ETS),
	case base_env_ets:get(platformfile,"") of
		[]-> ignor;
		FileName->
			base_env_ets:read_from_file(FileName,?OPTION_ETS)
	end.
	