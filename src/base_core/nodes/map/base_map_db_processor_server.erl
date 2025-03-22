%% Description: TODO: Add description to base_map_db_processor_server
-module(base_map_db_processor_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/2,
	whereis/1,
	query_db_name/1,
	query_map_stand/2,
	get_map_data/1,
	query_born_pos/1,
	query_safe_grid/2
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(View, 15).
-define(MapRect, {{0,0}, {50, 50}}).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {mapdb}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_server_shared.hrl").
-include("map_define.hrl").

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------
%%% put(key, value)、get(key)在进程字典中存储和检索键值对
%%% 进程字典是一个进程独有的、存储键值对的数据结构
%% --------------------------------------------------------------------
start_link(MapFile,MapId)->
	?base_gen_server:start_link(?MODULE ,[MapFile,MapId], []).

whereis(MapId)->
	MapDbProc = base_map_db_util:make_db_proc(MapId),
	case erlang:whereis(MapDbProc) of
		undefined->undefined;
		_Pid->MapDbProc
	end.

query_db_name(MapDbProc)->
	Reply = ?base_gen_server:call(MapDbProc, {query_db_name}),
	case Reply of
		{ok,DbName}->DbName;
		_->undefined
	end.

query_safe_grid(MapDb,{GridX,GridY})->
	case ?base_ets:lookup(MapDb, {sg,GridX,GridY}) of
		[]->
			0;
		[{_,Value}]->
			Value
	end.
	
query_born_pos(MapDb)->
	case ?base_ets:lookup(MapDb, born_pos) of
 		[PosInfo] ->
 			{born_pos,BornInfo} = PosInfo,
			BornInfo;
		[] ->
 			[]
 	end.
 	
query_map_stand(MapDbName,{X,Y})->
 	case ?base_ets:lookup(MapDbName, Y) of
 		[{Y,MaxX,StandBin}] ->
			if
				X>=MaxX->
					?MAP_DATA_TAG_CANNOT_WALK;
				true->
					PassX = X*8,
					<<_:PassX,Stande:8,_/binary>> = StandBin,					
						Stande
			end;
		[] ->
 			?MAP_DATA_TAG_NORMAL	%%
 	end.

get_map_data(Map_id) ->
	MapDbProc = ?MODULE:whereis(Map_id),
	MapDbName = query_db_name(MapDbProc),
	[{_,Data}] = ?base_ets:lookup(MapDbName, map_data),
	Data.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([MapFile,MapId]) ->
%%	case {ok,{{0,0},true}} of % file:consult(MapFile)
%%		{error,Reson}-> {stop,Reson};

%%		{ok,L}-> 
			MapDB = base_map_db_util:make_db_name(MapId),
			?base_ets:new(MapDB, [set,named_table]),	%% first new the database, and then register proc
			base_logger_util:info_msg("base_map_db_util:load_map_file MapId ~p MapDB~p ~n",[MapId,MapDB]),
			base_map_db_util:load_map_file(MapId,MapDB),
			register(base_map_db_util:make_db_proc(MapId),self()),
			{true, Tree} = build_quadtree(MapFile),
			?base_ets:insert(MapDB, {map_data, Tree}),
			{ok, #state{mapdb=MapDB}}.
%%	end.

?handle_call(Request, {From, Tag}, State) ->
	case Request of
		{query_db_name}-> {reply, {ok,State#state.mapdb}, State};
		_ -> {reply, ok, State}
	end.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
build_quadtree(_MapFile) ->
	%%	{Rect, View} = file:consults(MapFile),
	quadtree:build(?MapRect, ?View).
