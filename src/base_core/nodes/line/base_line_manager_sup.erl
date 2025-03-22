%% Description: TODO: Add description to base_line_manager_sup
-module(base_line_manager_sup).

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
-include("base_line_def.hrl").

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
	%% LineProcDB: store the line server(s)'s information.
	?base_ets:new(?ETS_LINE_PROC_DB, [set, public, named_table]),
	%% MapManagerDB: store the map manager's node information
	?base_ets:new(?ETS_MAP_MANAGER_DB, [set, public, named_table]),
	%% MapLineDB: store the one map's user count of all lines.
	?base_ets:new(?ETS_MAP_LINE_DB, [set, public, named_table]),
	%% ChatMaagerDB
	?base_ets:new(?ETS_CHAT_MANAGER_DB, [set, public, named_table]),
	
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([]) ->
	Manager = {base_line_manager_server,{base_line_manager_server,start_link,[]},
		   permanent,2000,worker,[base_line_manager_server]},
	{ok,{{one_for_one, 10, 10}, [Manager]}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
