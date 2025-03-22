%% Description: TODO: Add description to base_map_db_processor_sup
-module(base_map_db_processor_sup).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	start_map_db_processor/2
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
	supervisor:start_link({local, ?SERVER},?MODULE, []).

start_map_db_processor(MapFile,MapId)->
	MapDbProcTag = make_tag(MapId),
	ChildSpec = {MapDbProcTag,{base_map_db_processor_server,start_link,[MapFile,MapId]},
			  				permanent,2000,worker,[base_map_db_processor_server]},
	supervisor:start_child(?MODULE, ChildSpec).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([]) ->
	{ok,{{one_for_one,10,10}, []}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
make_tag(MapId)->
	list_to_atom(lists:append([integer_to_list(MapId),"_db"])).
