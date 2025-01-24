-module(base_db_master_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(CHECK_SPLIT_TABLE_FIRSTINTERVAL,1000).
-define(CHECK_SPLIT_TABLE_INTERVAL,1000*60*10).
-define(DEFAULT_BACKUP_INTERVAL,60*60*1000).

-define(DAL_WRITE_INTERVAL,60*1000).
-define(DAL_WRITE_CHECK_INTERVAL,10*1000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,rpc_add_self_to_db_node/2,rpc_add_self_to_dbslave_node/1,is_db_prepread/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

rpc_add_self_to_db_node(Node,TabList)->
	base_rpc_util:cast(Node, ?SERVER, {add_db_ram_node, [node(),TabList]}).

rpc_add_self_to_dbslave_node(Node)->
	base_rpc_util:cast(Node, ?SERVER, {add_dbslave_node, [node()]}).

is_db_prepread(Node)->
	try
		gen_server:call({?SERVER,Node}, is_db_prepread)
	catch
		E:R->
			base_logger_util:msg("get_db_master error no_proc ,wait ~n "),
			false
	end.

%% ====================================================================
%% Server functions
%% ====================================================================
start_link()->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	put(db_prepare_finish,false),
	base_timer_server:start_at_process(),

	
	base_timer_util:send_after(?CHECK_SPLIT_TABLE_FIRSTINTERVAL, {check_split}),
	base_db_ini_util:db_init_master(),
	% 定时检查备份
	send_check_dump_message(),

	% 初始化记录用的ets
	base_db_split_util:create_ets(),
	% 记录自定义类型disc_split的表 通过db_operater_behaviour搜集
	base_db_split_util:check_split_master_tables(),

	put(dbfile_dump,{idle,base_timer_server:get_correct_now()}),
	% base_db_dal_util:init(),	
	put(db_prepare_finish,true),
	{ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(is_db_prepread, From, State) ->
    Reply = get(db_prepare_finish),
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({check_backup},State)->
	%% get backup filename
	Dir = env:get2(dbback, output,[]),
	case Dir of
		[]-> ignor;
		_->
			%%check_today_dump(Dir),
			check_db_dump(Dir),
		   send_check_dump_message()
	end,
	{noreply,State};
handle_info({check_split},State)->
	ServerList = env:get(serverids, [0]),
	Result = lists:foldl(fun(ServerId,Acc)->
								 case Acc of
									 ignor->ignor;
									 ok->
										 base_db_split_util:check_need_split(ServerId)
								 end
						 end, ok,ServerList),	
	base_db_split_util:check_split_master_tables(),
	case Result of
		ok->erlang:send_after(?CHECK_SPLIT_TABLE_INTERVAL, self(), {check_split});
		ignor->erlang:send_after(?CHECK_SPLIT_TABLE_FIRSTINTERVAL, self(), {check_split})
	end,
	{noreply,State};
handle_info({add_db_ram_node, [NewNode,TabList]}, State)->
	base_db_tools:add_db_ram_node(NewNode,TabList),
	{noreply,State};

handle_info({add_dbslave_node, [NewNode]}, State)->
	base_db_tools:add_dbslave_node(NewNode),
	{noreply,State};

handle_info({backupdata,FromProc}, State)->
	base_logger_util:msg("backupdata begin~n"),
	case base_db_dal_util:read_rpc(role_pos) of
		{ok,L}->
			OnlineNum = length(L);
		_->
			OnlineNum = 0
	end,
	if
		OnlineNum =:= 0->	
			case base_db_dal_util:get_write_flag() of
				undefined->
					BackupFlag = true;
				WriteTime->
					BackupFlag = timer:now_diff(os:timestamp(),WriteTime) > ?DAL_WRITE_INTERVAL 
			end;
		true->
			BackupFlag = false
	end,
	if
		BackupFlag->		
			BackDir = env:get2(dbback, output,[]),
			BackPath = BackDir ++ "zybackup_db",
			data_gen:backup_ext(BackPath),
			base_rpc_util:cast(FromProc,{backup_db_ok}),
			base_logger_util:msg("data_gen backup db finish!!!");
		true->
			erlang:send_after(?DAL_WRITE_CHECK_INTERVAL, self(), {backupdata,FromProc})
	end,
	{noreply,State};


handle_info({backupdata}, State)->
	case base_db_dal_util:read_rpc(role_pos) of
		{ok,L}->
			OnlineNum = length(L);
		_->
			OnlineNum = 0
	end,
	if
		OnlineNum =:= 0->	
			case base_db_dal_util:get_write_flag() of
				undefined->
					BackupFlag = true;
				WriteTime->
					BackupFlag = timer:now_diff(os:timestamp(),WriteTime) > ?DAL_WRITE_INTERVAL 
			end;
		true->
			BackupFlag = false
	end,
	if
		BackupFlag->		
			BackDir = env:get2(dbback, output,[]),
			BackPath = BackDir ++ "zybackup_db",
			data_gen:backup_ext(BackPath),
			server_control:write_flag_file(),
			base_logger_util:msg("data_gen backup db finish!!!");
		true->
			erlang:send_after(?DAL_WRITE_CHECK_INTERVAL, self(), {backupdata})
	end,
	{noreply,State};

handle_info({recoverydata,FromProc},State)->
	base_logger_util:msg("recoverydata start~n"),
	BackDir = env:get2(dbback, output,[]),
	BackPath = BackDir ++ "zybackup_db",
	data_gen:recovery_ext(BackPath),
	data_gen:start(),
	data_gen:import_config("game"),
	base_logger_util:msg("data_gen recovery db finish!!!"),
	base_rpc_util:cast(FromProc,{recoverydata_ok}),
	{noreply,State};


handle_info({recoverydata},State)->
	base_db_tools:wait_for_all_db_tables(),
	BackDir = env:get2(dbback, output,[]),
	BackPath = BackDir ++ "zybackup_db",
	data_gen:recovery_ext(BackPath),
	data_gen:start(),
	data_gen:import_config("game"),
	server_control:write_flag_file(),
	base_logger_util:msg("data_gen recovery db finish!!!"),
	{noreply,State};

handle_info({gen_data},State)->
	%%data_gen:start(),
	base_db_tools:wait_for_all_db_tables(),
	data_gen:import_config("game"),
	%%ServerId = env:get(serverid,1),
	%%giftcard_op:import("../config/gift_card-"++integer_to_list(ServerId)++"01.config"),
	server_control:write_flag_file(),
	base_logger_util:msg("data_gen gen db finish!!!"),
	{noreply,State};

handle_info({create_giftcard},State)->
	%%create giftcard and import to db
	base_db_tools:wait_for_all_db_tables(),
	giftcard_op:auto_gen_and_import(),
	server_control:write_flag_file(),
	base_logger_util:msg("create_giftcard gen db finish!!!"),
	{noreply,State};

handle_info({format_data,Param},State)->
	base_db_tools:wait_for_all_db_tables(),
	data_change:update_db_data(Param),
	server_control:write_flag_file(),
	base_logger_util:msg("format_data ~p finish!!!~n",[Param]),
	{noreply,State};

handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	base_logger_util:msg("dbmaster terminate Reason ~p ~n",[Reason]),
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


send_check_dump_message()->
	CheckInterval = env:get2(dbback, checkinterval , []),
	if is_integer(CheckInterval)->
		   base_timer_util:send_after(CheckInterval, {check_backup});
	   true-> ignor
	end.
	


%%
%% for back up 
dum_now(Dir)->
	case Dir of
		[]-> ignor;
		_-> case filelib:ensure_dir(Dir) of
			   ok->File = get_out_file(Dir),
				   data_gen:backup(File);
			   _-> base_logger_util:msg("back database faild create dir [~p] failed!",[Dir])
		   end
	end.

check_today_dump(Dir)->
	Now = base_timer_server:get_correct_now(),
	{{Y,M,D},{H,_Min,_S}} = calendar:now_to_local_time(Now),
	%% check time
	Res = case env:get2(dbback, between_hour, []) of
		[]-> false;
		{B,E}-> 
			{NewB,NewE} = if B>E -> {E,B};
							 true-> {B,E}
						  end,
			if (H >= NewB) and (H =< NewE) ->
				   true;
			   true-> false
			end
	end,
	TodayFileHeader = "zyback_" ++ util:make_int_str4(Y) ++ "_" 
								++ util:make_int_str2(M) ++ "_" 
								++ util:make_int_str2(D) ++ "_",
	CheckNeedDump = case Res of
						true -> case file:list_dir(Dir) of
									{ok,FileNames}->
										lists:foldl(fun(FileName,Checked)->
															case Checked of
																false-> false;
																true->
																	case string:str(FileName, TodayFileHeader) of
																		0-> true;
																		1-> false;
																		_-> true
																	end
															end
													end,true, FileNames);
									_-> true
								end;
						_-> false
					end,
	
	case CheckNeedDump of
		true->
			dum_now(Dir);
		false->
			ignor
	end.

db_dump_now(Dir,LastTime)->
	case Dir of
		[]-> ignor;
		_-> case filelib:ensure_dir(Dir) of
			   ok->
					put(dbfile_dump,{backup,LastTime}),
				   	File = get_out_file(Dir),
				   	data_gen:backup_ext(File),
				    put(dbfile_dump,{idle,base_timer_server:get_correct_now()});
			   _-> base_logger_util:msg("back database faild create dir [~p] failed!",[Dir])
		   end
	end.

check_db_dump(Dir)->
	case get(dbfile_dump) of
		undefined->
			nothing;
		{backup,_}->
			nothing;
		{idle,LastTime}->
			Now = base_timer_server:get_correct_now(),
			BackInterval = env:get2(dbback,backinterval,?DEFAULT_BACKUP_INTERVAL),
			TimeDiff = trunc(timer:now_diff(Now,LastTime)/1000),
			if
				BackInterval =< TimeDiff->
					db_dump_now(Dir,LastTime);
				true->
					nothing
			end;
		_->
			nothing
	end.

get_out_file(OutDir)->
	Now = base_timer_server:get_correct_now(),
	{{Y,M,D},{H,Min,S}} = calendar:now_to_local_time(Now),
	File = string:join(["zyback",
							util:make_int_str4(Y),	
							util:make_int_str2(M),
							util:make_int_str2(D),
							%%util:make_int_str4(S+Min*60+H*3600)
							util:make_int_str6(H*10000+Min*100+S) 
							],
							"_"),
	OutDir ++ File.
	
