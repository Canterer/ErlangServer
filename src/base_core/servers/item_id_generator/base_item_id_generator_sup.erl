%% Description: TODO: Add description to base_item_id_generator_sup
-module(base_item_id_generator_sup).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0
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
start_link()->	
	ServerId=base_env_ets:get(serverid,undefined),
	case ServerId of
		undefined-> 
			Error="can not start line,there is not serverid in configfile\n",
			?base_logger_util:info_msg(Error),
			{error,Error};
		_->
			supervisor:start_link({local, ?SERVER}, ?MODULE, [ServerId])
	end.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([ServerId]) ->
	ItemIdConfig = {base_item_id_generator_server,
						{base_item_id_generator_server,start_link,[ServerId]},
						permanent,2000,worker,[base_item_id_generator_server]},
	{ok,{{one_for_one, 10, 10}, [ItemIdConfig]}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------