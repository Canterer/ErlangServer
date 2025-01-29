-module(base_map_manager_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common_define.hrl").
-define(GAME_MAP_MANAGER,local_map_manager).
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,start_map_processor/4,stop_map_processor/3,start_instance/3,stop_instance/2,make_map_process_name/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).


%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
	gen_server:start_link({local,?GAME_MAP_MANAGER},?MODULE,[],[]).

%% ====================================================================
%% Server functions
%% ====================================================================

start_map_processor(MapManagerNode,LineId,MapId,Tag)->
	base_logger_util:msg("~p:~p(MapManagerNode:~p,LineId:~p,MapId:~p,Tag:~p)~n",[?MODULE,?FUNCTION_NAME,MapManagerNode,LineId,MapId,Tag]),
	base_rpc_util:cast(MapManagerNode, ?GAME_MAP_MANAGER ,{start_map_process,LineId,MapId,Tag}).

stop_map_processor(MapManagerNode,LineId,MapId)->
	base_logger_util:msg("~p:~p(MapManagerNode:~p,LineId:~p,MapId:~p)~n",[?MODULE,?FUNCTION_NAME,MapManagerNode,LineId,MapId]),
	base_rpc_util:cast(MapManagerNode, ?GAME_MAP_MANAGER , {stop_map_process,LineId,MapId}).

start_instance(MapName,CreatInfo,MapId)->
	base_logger_util:msg("~p:~p(MapName:~p,CreatInfo:~p,MapId:~p)~n",[?MODULE,?FUNCTION_NAME,MapName,CreatInfo,MapId]),
	try
		gen_server:call(?GAME_MAP_MANAGER,{start_instance,MapName,CreatInfo,MapId})
	catch
		E:R ->
			% instanceid_generator:safe_turnback_proc(MapName),
			base_logger_util:msg("map_manager:start_instance error: ~p ~p,MapName ~p,ProtoId ~p,MapId ~p",[E,R,MapName,CreatInfo,MapId]),
			error
	end.

stop_instance(MapManagerNode,MapName)->
	base_logger_util:msg("~p:~p(MapManagerNode:~p,MapName:~p)~n",[?MODULE,?FUNCTION_NAME,MapManagerNode,MapName]),
	base_rpc_util:cast(MapManagerNode, ?GAME_MAP_MANAGER , {stop_instance,MapName}).
		
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}		  |
%%		  {ok, State, Timeout} |
%%		  ignore			   |
%%		  {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	%% start time to check 
	send_check_message(),
	base_logger_util:msg("~p:line:~p~n",[?MODULE,?LINE]),
	%%load all map
	% 从mnesia数据库中读取所有地图配置数据
	AllMapInfo = base_map_info_db:get_all_maps_and_serverdata(),
	base_logger_util:msg("~p:line:~p AllMapInfo:~p~n",[?MODULE,?LINE,AllMapInfo]),
	lists:foreach(fun({MapId,MapDataId})->
		MapDb = base_map_db_util:make_db_name(MapId),
		base_logger_util:msg("~p:line:~p {MapId:~p,MapDataId:~p} MapDb:~p~n",[?MODULE,?LINE,MapId,MapDataId,MapDb]),
		case ets:info(MapDb) of
			undefined->
				base_logger_util:msg("~p:line:~p~n",[?MODULE,?LINE]),
				ets_operater_behaviour:new(MapDb, [set,named_table]),	%% first new the database, and then register proc
				case MapDataId of
					[]->
						nothing;
					_->
						base_map_db_util:load_map_ext_file(MapDataId,MapDb),
						base_map_db_util:load_map_file(MapDataId,MapDb)
				end;
			_->
				base_logger_util:msg("~p:line:~p~n",[?MODULE,?LINE]),
				nothing
		end end,AllMapInfo),
%%	DefaultLoadMapIDs = [?DEFAULT_MAP|base_env_ets:get(preload_map,undefined)],
%%	lists:foreach(fun(MapId)->
%%		MapDb = base_map_db_util:make_db_name(MapId),
%%		case ets:info(MapDb) of
%%			undefined->
%%				ets_operater_behaviour:new(MapDb, [set,named_table]),	%% first new the database, and then register proc
%%				base_map_db_util:load_map_ext_file(MapId,MapDb),
%%				base_map_db_util:load_map_file(MapId,MapDb);
%%			_->
%%				nothing
%%		end end,DefaultLoadMapIDs),
	erlang:garbage_collect(),
	base_logger_util:msg("~p:line:~p~n",[?MODULE,?LINE]),
	{ok, #state{}}.

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
handle_call({start_instance,MapName,CreatInfo,MapId}, _From, State) ->
	base_logger_util:msg("~p:~p({start_instance,MapName:~p,CreatInfo:~p,MapId:~p}, _From:~p, State:~p)~n",[?MODULE,?FUNCTION_NAME,MapName,CreatInfo,MapId,_From,State]),
	case base_map_processor_sup:start_child(MapName,{-1,MapId},CreatInfo) of
		{ok,Pid} ->
			base_logger_util:msg("---start map ok \n"),
			Reply = ok;
		{ok,Pid,_Info} ->
			base_logger_util:msg("---start map ok info ~p \n",[_Info]),
			Reply = ok;
		{error,Error} ->
			base_logger_util:msg("---start map failed, reason: ~p\n", [Error]),
			Reply = error,
			Pid = 0
	end,		
	{reply, Reply, State}.

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
handle_info({global_line_check}, State) ->
	base_logger_util:msg("~p:~p({global_line_check}, State:~p)~n",[?MODULE,?FUNCTION_NAME,State]),
	case base_line_manager_server:whereis_name() of
		error->
			base_logger_util:msg("lines manager can not found ,1 sencond check\n"),
			send_check_message();
		undefined ->
			base_logger_util:msg("lines manager can not found ,1 sencond check\n"),
			send_check_message();
		_GlobalPid-> 
			base_logger_util:msg("send to base_line_manager_server {~p,~p}\n",[node(),?GAME_MAP_MANAGER]),
			base_line_manager_server:regist_map_manager({node(),?GAME_MAP_MANAGER})
	end,
	{noreply, State};
		
handle_info({start_map_process,LineId,MapId,Tag}, State) ->	
	base_logger_util:msg("~p:~p({start_map_process,LineId:~p,MapId:~p,Tag:~p}, State:~p)~n",[?MODULE,?FUNCTION_NAME,LineId,MapId,Tag,State]),	
	MapName = make_map_process_name(LineId,MapId),			
	case base_map_processor_sup:start_child(MapName,{LineId,MapId},Tag) of
		{ok,Child} ->
				base_line_manager_server:regist_map_processor({node(), LineId, MapId, MapName});
		{ok,Child,Info} ->
				base_line_manager_server:regist_map_processor({node(), LineId, MapId, MapName});
		{error,Error} ->
			base_logger_util:msg("---start map failed, reason: ~p\n", [Error])
	end,
	{noreply, State};

handle_info({stop_map_process,LineId,MapId}, State) ->
	MapName = make_map_process_name(LineId,MapId),
	base_map_processor_sup:stop_child(MapName),	
	{noreply, State};

handle_info({stop_instance,MapName}, State) ->
	base_map_processor_sup:stop_child(MapName),	
	{noreply, State};

handle_info({change_map_bornpos,MapId,BronMap,BornX,BornY},State)->
	try
		MapDb = base_map_db_util:make_db_name(MapId),
		ets_operater_behaviour:insert(MapDb,{born_pos,{BronMap,{BornX,BornY}}})
	catch
		E:R->
			base_logger_util:msg("change_map_bornpos error E:~p R:~p ~n",[E,R])
	end,
	{noreply, State};

handle_info(_INFO, State) ->
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
%%% Internal functions
%% --------------------------------------------------------------------

send_check_message()->
	base_timer_util:send_after(1000, {global_line_check}).


make_map_process_name(LineId,MapId)->
	ListMap = lists:append(["map_",integer_to_list(LineId),"_",integer_to_list(MapId)]),
	list_to_atom(ListMap).

