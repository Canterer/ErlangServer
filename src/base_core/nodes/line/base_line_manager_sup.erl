-module(base_line_manager_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_line_def.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	 start_link/0
	]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

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

start_link() ->
    %% LineProcDB: store the line server(s)'s information.
    ets:new(?ETS_LINE_PROC_DB, [set, public, named_table]),
    %% MapManagerDB: store the map manager's node information
    ets:new(?ETS_MAP_MANAGER_DB, [set, public, named_table]),
    %% MapLineDB: store the one map's user count of all lines.
    ets:new(?ETS_MAP_LINE_DB, [set, public, named_table]),
    %% ChatMaagerDB
    ets:new(?ETS_CHAT_MANAGER_DB, [set, public, named_table]),
    
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    Manager = {base_lines_manager,{base_lines_manager,start_link,[]},
	       permanent,2000,worker,[base_lines_manager]},
    {ok,{{one_for_one, 10, 10}, [Manager]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
