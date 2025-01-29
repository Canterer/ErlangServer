-module(base_map_processor_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,start_child/3,stop_child/1]).

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
start_link()->
	supervisor:start_link({local,?SERVER}, ?MODULE, []).


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
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	{ok,{{one_for_one,10,10}, []}}.

start_child(MapProcName,MapId_line,Tag)->
	base_logger_util:msg("~p:~p(MapProcName:~p,MapId_line:~p,Tag:~p)~n",[?MODULE,?FUNCTION_NAME,MapProcName,MapId_line,Tag]),
	try
		AChild = {MapProcName,{base_map_processor_server,start_link,[MapProcName,{MapId_line,Tag}]},
				  		  		transient,2000,worker,[base_map_processor_server]},
		supervisor:start_child(?SERVER, AChild)
	catch
		E:R-> base_logger_util:msg("can not start map(~p:~p) ~p ~p ~p~n",[E,R,MapProcName,MapId_line,Tag]),
			  {error,R}
 	end.

stop_child(MapProcName)->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	case ets:info(MapProcName) of
		undefined->
			nothing;
		_->
			ets_operater_behaviour:delete(MapProcName)
	end,
	supervisor:terminate_child(?SERVER, MapProcName),
	supervisor:delete_child(?SERVER, MapProcName).
%% ====================================================================
%% Internal functions
%% ====================================================================

