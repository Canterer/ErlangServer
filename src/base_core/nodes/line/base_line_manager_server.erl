-module(base_line_manager_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_line_def.hrl").
%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_lines_manager interface
-export([
	add_line/1,
	wait_lines_manager_loop/0,
	delete_line/1,
	regist_map_manager/1,
	whereis_name/0,
	lookup_map_manager/1,
	regist_map_processor/1,
	regist_map_processor/2,
	query_line_status/3,
	get_line_status/1,
	get_map_name/2,
	get_map_node/2,
	get_map_nodes/0,
	regist_to_manager/2,
	unregist_map_by_node/1,
	get_line_map_in_node/1,
	get_role_num_by_mapId/0,
	open_dynamic_line/1
]).


%% gen_server callbacks
-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2, 
	terminate/2, 
	code_change/3
]).

%%add for chat
% -export([regist_chatmanager/1,get_chat_name/0]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link()->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

%% Dynamic adding line server
%% LineId: The line server' name, such as 'Line#1'
%% @spec add_line(atom()) -> Pid.
open_dynamic_line(Line)->
	base_global_proc_util:send(?SERVER,{add_dynamic_line,Line}).

add_line(LineId) ->
	base_global_proc_util:send(?SERVER, {add_line_server, LineId}).

%% Description: Dynamic deleteing line server
%% LineId: The line server' name, such as 'Line#1'
%% @spec delete_line(atom()) -> Pid.
delete_line(LineId) ->
	base_global_proc_util:send(?SERVER, {delete_line_server, LineId}).

%% --------------------------------------------------------------------
%%% Description: regist this line server into line manager
%%% LineName: this line server' name
%%% LinesManagerName: the line manager' name
%% --------------------------------------------------------------------
%% @spec regist_to_manager(atom(), atom()) -> Pid().
regist_to_manager(LinesManagerName,LineName) ->
	base_global_proc_util:send(LinesManagerName, {regist_line_server, LineName}).

%% Description: Dynamic deleteing line server
%% Args: {node_name, map_center_name}
%% @spec regist_map_manager(tupe()) -> Pid.
regist_map_manager(Args) ->
	base_global_proc_util:send(?MODULE, {regist_map_manager, Args}).	

% %%
% %%regist chatmagager
% %%
% regist_chatmanager(Args) ->
% 	base_global_proc_util:send(?MODULE, {regist_chatmanager, Args}).

wait_lines_manager_loop()->
	case wait_lines_manager() of
		true->
			true;
		_->
			timer:sleep(1000),
			base_logger_util:msg("wait_lines_manager_loop ~n"),
			wait_lines_manager_loop()		
	end.	

wait_lines_manager()->
	try
		base_global_proc_util:call(?MODULE,wait_lines_manager)
	catch
		E:R->
			base_logger_util:msg("wait_lines_manager whereis_name() ~p ~p ~n",[E,R]),
			error
	end.	
%% Description: check the line manager whether exists.
%% @spec whereis_name() -> Pid()|undefined.
whereis_name() ->
	try
		base_global_proc_util:call(?MODULE,{whereis_name})
	catch
		E:R->
			base_logger_util:msg("lines_manager whereis_name() ~p ~p ~n",[E,R]),
			error
	end.


%% Description: lookup the map_processor' node whether is registed.
%% Node: the node that you want to lookup.
%% @spec lookup_map_manager(atom()) -> Reply.
lookup_map_manager(Node) ->
	base_global_proc_util:call(?MODULE,{lookup_map_manager, Node}).

%% Description: registed the map information
%% Args: the regist information, such as {NodeName, LineName, MapName}
%% @spec regist_map_processor(atom()) -> void.
regist_map_processor(Args) ->
	base_global_proc_util:call(?MODULE, {regist_map_processor, Args},infinity).

regist_map_processor(Node,Args)->
	try
		gen_server:call({?MODULE,Node}, {regist_map_processor, Args})
	catch
		E:R->
			base_logger_util:msg("regist_map_processor Node ~p Info ~p E ~p R ~p ~n ",[Node,Args,E,R]),
			error
	end.

unregist_map_by_node(Node)->
	base_global_proc_util:send(?MODULE, {unregist_map_by_node, Node}).
  
%% Description: query all lines that own the mapid.
%% FromNode: the query processor's node
%% FromProcName: the query process's name
%% Mapid: the mapid that you want
%% @spec query_line_status(atom(), atom(), atom()) -> void

%% Description: query all lines role-count.
%% FromNode: the query processor's node
%% FromProcName: the query process's name
%% @spec query_line_status(atom(), atom(), ) -> void()
query_line_status(FromNode, FromProcName, Mapid) ->
	base_global_proc_util:send(?MODULE, {get_role_count_by_map, {FromNode, FromProcName, Mapid}}).

get_line_status(MapId)->
	base_global_proc_util:call(?MODULE, {get_line_status,MapId}).
	
%% Description: get the map's proc name
%% Mapid: the Map's id
%% LineId: the Line's id
%% return: {ok, MapProcName}|{error}
get_map_name(LineId, MapId) ->
	base_global_proc_util:call( ?MODULE, {get_map_name, {LineId, MapId}}).

get_map_node(LineId, MapId)->
	base_global_proc_util:call( ?MODULE, {get_map_node, {LineId, MapId}}).

get_map_nodes()->
  	base_global_proc_util:call( ?MODULE, {get_all_map_node}).

%%[{lineid,mapid}]
get_line_map_in_node(MapNode)->
  	base_global_proc_util:call( ?MODULE, {get_line_map_in_node,MapNode}).
%% get chat node
get_chat_name()->		
	base_global_proc_util:call( ?MODULE, {get_chat_name}).

%%
%%
%%
get_role_num_by_mapId()->
	base_global_proc_util:call(?MODULE, {get_role_num_by_mapId}).

	
%% ====================================================================
%% Server functions
%% ====================================================================

%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}		  |
%%		  {ok, State, Timeout} |
%%		  ignore			   |
%%		  {stop, Reason}
init([]) ->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	start_line_server(),
	{ok, #state{}}.


%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}		  |
%%		  {reply, Reply, State, Timeout} |
%%		  {noreply, State}			   |
%%		  {noreply, State, Timeout}	  |
%%		  {stop, Reason, Reply, State}   | (terminate/2 is called)
%%		  {stop, Reason, State}			(terminate/2 is called)
handle_call(Request, From, State) ->
	Reply = 
	try
		case Request of
			wait_lines_manager->
				true;
			{lookup_map_manager, Node} ->
				check_map_manager_node(Node);
			{get_all_map_node}->
				get_all_map_node();
			{get_map_node,{LineId, MapId}}->
				case base_line_processor_server:lookup_map_name(LineId, MapId) of
					{error}->
						error;
					{ok,{Node,_}}->
						Node
				end;
			{get_map_name, {LineId, MapId}} ->
				base_line_processor_server:lookup_map_name(LineId, MapId);
			{get_chat_name} ->			
				lookup_chat_manager();
			{get_line_status,MapId}->		
				LineInfo = base_line_processor_server:get_role_count_by_map(MapId),
				LineInfo;
			{get_role_num_by_mapId} ->
				base_line_processor_server:get_role_num_by_mapId();
			{whereis_name}->
				erlang:whereis(?MODULE);
			{regist_map_processor, Args}->
				base_line_processor_server:do_regist(regist_map_processor, Args, State);
			{get_line_map_in_node,MapNode}->
				proc_get_line_map_in_node(MapNode);
			_ ->
				ok
		end
	catch 
		E:R->
			base_logger_util:msg("lines manager handle_call error ~p ~p ~p ~n",[E,R,erlang:get_stacktrace()]),
			error
	end,
	{reply, Reply, State}.

%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
handle_cast(Msg, State) ->
	{noreply, State}.


%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)

handle_info(Info, State) ->
	try
		case Info of
			%% regist line proc need 2 step,
			%% unregist line proc only need 1 step.
			%% Why? we use the rule: if we can look up the line proc's name,
			%% it must be vaild.
			{add_dynamic_line,Line}->
				case lists:member(Line, get(dynamic_lines) ) of
					true->
						nothing;
					_->
						put(dynamic_lines,[Line|get(dynamic_lines)])
				end;
			{add_line_server, LineName} ->
				case line_is_exist(LineName) of
					false ->
						base_base_line_processor_server_sup:add_line({LineName, ?SERVER});
					true ->
						base_logger_util:msg("~p is exist~n", [LineName])
				end;
			{regist_line_server, LineName} ->
				ets_operater_behaviour:insert(?ETS_LINE_PROC_DB, {LineName});
	
			{delete_line_server, LineName} ->
				base_base_line_processor_server_sup:delete_line(LineName),
				ets_operater_behaviour:delete(?ETS_LINE_PROC_DB,LineName);
	
			{regist_map_manager, {Node, Name}} ->
				%% insert the map_manager's name into map_manager db, formate of
				%% information: {Node, map_managerName}
				ets_operater_behaviour:insert(?ETS_MAP_MANAGER_DB, {Node, Name}),
				%% load map data
				load_map(Node);
			
			%% regist chatmanager
			{regist_chatmanager, {Node, Name, Count}} ->
				ets_operater_behaviour:insert(?ETS_CHAT_MANAGER_DB, {Node, Name,Count});
			
			{get_role_count_by_map, {FromNode, FromProcName, MapId}} ->
				LineInfo = base_line_processor_server:get_role_count_by_map(MapId),
				tcp_client:line_info_success(FromNode, FromProcName, LineInfo);
			
			{line_load_map, {LineName, MapNodeName}}->
				start_map_processor(LineName, MapNodeName);
			{unregist_map_by_node,NodeName}->
			  	base_line_processor_server:unregist_by_node(NodeName);
			_ ->
				base_logger_util:msg("receive message: {~p}~n", [Info])		
		end
	catch 
		E:R->
			base_logger_util:msg("lines manager handle_info error ~p ~p ~p ~n",[E,R,erlang:get_stacktrace()])
	end,		
	{noreply, State}.

%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
terminate(_Reason, State) ->
	ok.


%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
code_change(OldVsn, State, Extra) ->
	{ok, State}.


%% ====================================================================
%%% Internal functions
%% ====================================================================

%% Description: start the line server.
%% @spec initial_line_server() -> void().
start_line_server() ->
	Lines = base_env_ets:get(lines, []),
	put(dynamic_lines,base_env_ets:get2(line_switch,dynamic,[])),
	lists:foreach(fun(H) -> add_line(H) end, Lines).


%% Description: Load static map data.
%% @spec load_map() -> void().
load_map(MapNode) ->
	Lines = base_env_ets:get(lines, []),
	MapConfigFlag = base_env_ets:get(mapconfig_flag, []),
	lists:foreach(fun(LineId) ->
					  start_map_processor(MapConfigFlag,LineId, MapNode)
			  end, Lines).

start_map_processor(LineId, MapNode)->
	MapConfigFlag = base_env_ets:get(mapconfig_flag, []),
	start_map_processor(MapConfigFlag, LineId, MapNode).
	
start_map_processor(?MAPCONFIG_FROM_DATA, LineId, MapNode)->
	% CheckLoad = 
	% 	case server_travels_util:is_share_map_node(MapNode) of
	% 		true->
	% 			true;
	% 		_->
	% 			base_node_util:check_match_map_and_line(MapNode,LineId)
	% 	end,
	CheckLoad = base_node_util:check_match_map_and_line(MapNode,LineId),
	if
		CheckLoad->
			%%get all map config from ets
			AllMaps = base_map_info_db:get_maps_bylinetag(LineId),
			lists:foreach(fun(MapId)->
					base_map_manager_server:start_map_processor(MapNode, LineId, MapId, map)					  
				end, AllMaps);
		true->
			nothing
	end;
	

start_map_processor(?MAPCONFIG_FROM_OPTION, LineId, MapNode) ->
	SNode = base_node_util:get_node_sname(MapNode),
	% SNode = base_node_util:get_match_snode(map,MapNode),
	Host = base_node_util:get_node_host(MapNode),
	AllMaps = base_env_ets:get(lines_info, []),
	case base_line_processor_server:get_map(AllMaps, LineId, {SNode,Host,MapNode}) of
		{true, MapInfos} ->
			lists:foreach(fun({MapName, NodeName}) ->
				base_map_manager_server:start_map_processor(NodeName, LineId, MapName, map)				  
			end, MapInfos);
		{false} ->
			ok
	end;

start_map_processor(MapConfigFlag, LineId, MapNode)->
	base_logger_util:msg("~p start_map_processor mapconfigflag ~p ~n",[?MODULE,MapConfigFlag]).
	

check_map_manager_node(Node) ->
	case ets:lookup(?ETS_MAP_MANAGER_DB, Node) of
		[] ->
			wrong;
		_ ->
			ok
	end.

get_all_map_node()->
	lists:map(fun({A,_B})-> A end,ets:tab2list(?ETS_MAP_MANAGER_DB)).

%% Description: check the line whether exists
%% @spec line_is_exist(atom(), atom()) -> true|false
line_is_exist(LineName) ->
	case ets:lookup(?ETS_LINE_PROC_DB, LineName) of
		[] ->
			false;   
		_ ->
			true
	end.

%% return cat node
lookup_chat_manager() ->
	List = ets:tab2list(?ETS_CHAT_MANAGER_DB),
	Fun = fun({Node,ProcName,Count},Last)->
				  case Last of
					  []-> {Node,ProcName,Count};
					  {Node0,Proc0,Count0}->
						  if Count0>Count ->
								 {Node,ProcName,Count};
							 true->
								 {Node0,Proc0,Count0}
						  end
				  end
		  end,
	lists:foldl(Fun, [], List).

%%return [{lineid,mapid}]
proc_get_line_map_in_node(MapNode)->
	AllMatchs = ets:match_object(?MAP_PROC_DB, {'_', MapNode, '_', '_', '_'}),
	lists:map(fun({_,_,_,LineId,MapId})->{LineId,MapId} end, AllMatchs).
	
	
	
