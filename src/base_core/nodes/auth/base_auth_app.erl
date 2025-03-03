%% Description: TODO: Add description to base_auth_app
-module(base_auth_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("reloader.hrl").

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	start/0,
	start/2,
	stop/1
]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

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
start()->
	base_application_server:start(?MODULE).

%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
	case base_node_util:get_argument('-line') of
		[]->  base_logger_util:msg("Missing --line argument input the nodename");
		[CenterNode|_]->
			% ?RELOADER_RUN,
			base_ping_util:wait_all_nodes_connect(),
			% case server_travels_util:is_share_server() of
			% 	false->
			% 		travel_deamon_sup:start_link();
			% 	true->
			% 		nothing
			% end,
			base_global_proc_util:wait_global_proc_register(),
			base_timer_server:start_at_app(),
			% statistics_sup:start_link(),
			case base_auth_sup:start_link() of
				{ok, Pid} ->
					nothing;
				Error ->
					Error
			end,
			{ok, self()}
	end.
	

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

