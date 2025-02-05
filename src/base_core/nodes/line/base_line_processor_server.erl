-module(base_line_processor_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_line_def.hrl").

-define(LOOKUP_INTERVAL, 100).

%% --------------------------------------------------------------------
%% External exports
-export([
	start_link/1,
	lookup_map_name/2,
	do_regist/3,
	unregist_by_node/1,
	get_map/3
]).
-export([
	get_role_count_by_map/1,
	get_role_num_by_mapId/0
]).

% -compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {line_name}).

%% ====================================================================
%% External functions
%% ====================================================================
%%	TODO:this proc not need again!!!!!!!
start_link({LineName, NamedProc})->
	base_gen_server:start_link({local, LineName}, ?MODULE, {LineName, NamedProc}, []).

% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}		  |
%%		  {ok, State, Timeout} |
%%		  ignore			   |
%%		  {stop, Reason}
%% --------------------------------------------------------------------
init({LineName, NamedProc}) ->
	base_logger_util:msg("~p:~p({LineName:~p, NamedProc:~p})~n",[?MODULE,?FUNCTION_NAME,LineName,NamedProc]),
	base_line_manager_server:regist_to_manager(NamedProc,LineName),
	{ok, #state{line_name = LineName}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}		  |
%%		  {reply, Reply, State, Timeout} |
%%		  {noreply, State}			   |
%%		  {noreply, State, Timeout}	  |
%%		  {stop, Reason, Reply, State}   | (terminate/2 is called)
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
	{reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Description: get the maps that belongs to this line server.
%%% AllMaps: the map data of all line server.
%%% LineName: this line server' name.
%%% Return: maps that belongs to this line server.
%% --------------------------------------------------------------------
%% @spec create_map(atom(), atom()) -> list().
get_map(AllMaps, LineName, {SNode,Host,MapNode}) ->
	get_map2(AllMaps, LineName, {SNode,Host,MapNode}).

get_map2([H|T], LineName, {SNode,Host,MapNode}) ->
	{Line, Maps} = H,
	case Line =:= LineName of
		true ->
			Fun = fun(MapRec) ->
						{_, Node} = MapRec,
						Node =:= SNode				  
				  end,
			Filt = lists:filter(Fun, Maps),
			MakeNode = lists:map(fun({MapId,X})-> 
										%% NodeStr = atom_to_list(X) ++"@" ++ Host,
										%% Node = list_to_atom(NodeStr),
										%% {MapId,Node}
										{MapId,MapNode}
								 end, Filt),
			{true, MakeNode};
		false ->
			get_map2(T, LineName, {SNode,Host,MapNode})
	end;
%% This line is empyt line, does not contain map
get_map2([], _LineName, _MapNodeName) ->
	{false}.

%% --------------------------------------------------------------------
%% Description: regist information into LineServer
%% Args: regist infromation
%% --------------------------------------------------------------------
%% @spec do_regist(atom(), atom()) -> void().
do_regist(regist_map_processor, Args, State) ->
	{NodeName, LineId, MapId, MapName} = Args,
	Key = make_map_id(LineId, MapId),
	%% {key, nodename, mapname, lineid, mapid, rolecount}
	%base_logger_util:msg("regist_map_processor ~p ~n ",[Args]),
	ets_operater_behaviour:insert(?MAP_PROC_DB, {Key, NodeName, MapName, LineId, MapId}),
	ok.

do_regist(RegistType, Args) ->
	%% unkonwn format
	{LineId, MapId} = RegistType,
	Id = make_map_id(LineId, MapId),
	base_logger_util:msg("unknown regist type: ~s~n", [Id]).

unregist_by_node(NodeName)->
	AllNodeMaps = ets:match(?MAP_PROC_DB, {'$1', NodeName, '_', '_', '_'}),
	lists:foreach(fun(Key)-> ets_operater_behaviour:delete(?MAP_PROC_DB, Key) end, lists:append(AllNodeMaps)).
	

%% Description: get the map processor name by LineId and MapId
lookup_map_name(LineId, MapId) ->
	Key = make_map_id(LineId, MapId),
	case ets:lookup(?MAP_PROC_DB, Key) of
		[{_, NodeName, ProcName, _, _}]->
			{ok, {NodeName, ProcName}};
		[] ->
			{error}
	end.

make_map_id(LineId, MapId) ->
	integer_to_list(LineId) ++"_"++ integer_to_list(MapId).

%% Description: get the rold count by mapid
%% return: [{LineId, Count}, {LineId, Count}]
%% DB field: {key, nodename, mapname, lineid, mapid, rolecount}
%%		   {'_', '_',	  '_',	 '_',	mapid, '$1'}).
get_role_count_by_map(MapId) ->
	todo.

%%
%%get role num in the map (all line)
%%return [{MapId,RoleNum},.....]
%%
get_role_num_by_mapId() ->
	todo.