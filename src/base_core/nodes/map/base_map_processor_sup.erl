%% Description: TODO: Add description to base_map_processor_sup
-module(base_map_processor_sup).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	start_child/3,
	stop_child/1
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

start_child(MapProcName,MapId_line,Tag)->
	?ZS_LOG("MapProcName:~p,MapId_line:~p,Tag:~p",[MapProcName,MapId_line,Tag]),
	try
		AChild = {MapProcName,{base_map_processor_server,start_link,[MapProcName,{MapId_line,Tag}]},
				  		  		transient,2000,worker,[base_map_processor_server]},
		supervisor:start_child(?SERVER, AChild)
	catch
		E:R-> base_logger_util:info_msg("can not start map(~p:~p) ~p ~p ~p~n",[E,R,MapProcName,MapId_line,Tag]),
			  {error,R}
 	end.

stop_child(MapProcName)->
	?ZS_LOG("MapProcName=~p",[MapProcName]),
	case ?base_ets:info(MapProcName) of
		undefined->
			nothing;
		_->
			?base_ets:delete(MapProcName)
	end,
	supervisor:terminate_child(?SERVER, MapProcName),
	supervisor:delete_child(?SERVER, MapProcName).
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([]) ->
	{ok,{{one_for_one,10,10}, []}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
