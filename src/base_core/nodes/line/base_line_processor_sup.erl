-module(base_line_processor_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_line_def.hrl").

%% API
-export([
	start_link/0,
	add_line/1,
	delete_line/1
]).

%% Supervisor callbacks
-export([init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
	ets_operater_behaviour:new(?MAP_PROC_DB, [set, public, named_table]),
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%					 ignore						  |
%%					 {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	{ok,{{one_for_one, 10, 10}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
add_line({LineName, From}) ->
	base_logger_util:msg("~p:~p({LineName:~p, From:~p)~n",[?MODULE,?FUNCTION_NAME,LineName,LineName,From]),
	supervisor:start_child(?SERVER, {LineName, {base_line_processor_server, start_link, [{LineName, From}]},
					 permanent, 2000, worker, [base_line_processor_server]}).

delete_line(LineName) ->
	base_logger_util:msg("~p:~p(LineName:~p)~n",[?MODULE,?FUNCTION_NAME,LineName]),
	supervisor:terminate_child(?SERVER, LineName),
	supervisor:delete_child(?SERVER, LineName).
