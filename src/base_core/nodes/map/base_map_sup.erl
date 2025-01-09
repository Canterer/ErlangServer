-module(base_map_sup).

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
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,{{one_for_one,10,10}, []}}.

start_child(MapProcName,MapId_line,Tag)->
	try
		AChild = {MapProcName,{base_map_processor_server,start_link,[MapProcName,{MapId_line,Tag}]},
				  	      		transient,2000,worker,[base_map_processor_server]},
		supervisor:start_child(?SERVER, AChild)
	catch
		E:R-> io:format("can not start map(~p:~p) ~p ~p ~p~n",[E,R,MapProcName,MapId_line,Tag]),
			  {error,R}
 	end.

stop_child(MapProcName)->
	case ets:info(MapProcName) of
		undefined->
			nothing;
		_->
			ets:delete(MapProcName)
	end,
	supervisor:terminate_child(?SERVER, MapProcName),
	supervisor:delete_child(?SERVER, MapProcName).
%% ====================================================================
%% Internal functions
%% ====================================================================

