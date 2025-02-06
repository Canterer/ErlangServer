%% Description: TODO: Add description to base_line_processor_sup
-module(base_line_processor_sup).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	add_line/1,
	delete_line/1
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
	?ZS_LOG(),
	ets_operater_behaviour:new(?MAP_PROC_DB, [set, public, named_table]),
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_line({LineName, From}) ->
	?ZS_LOG("{LineName:~p, From:~p",[LineName,From]),
	supervisor:start_child(?SERVER, {LineName, {base_line_processor_server, start_link, [{LineName, From}]},
					 permanent, 2000, worker, [base_line_processor_server]}).

delete_line(LineName) ->
	?ZS_LOG("LineName:~p",[LineName]),
	supervisor:terminate_child(?SERVER, LineName),
	supervisor:delete_child(?SERVER, LineName).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_init([]) ->
	{ok,{{one_for_one, 10, 10}, []}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
