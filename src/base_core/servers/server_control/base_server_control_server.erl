%% Description: TODO: Add description to xserver_control
-module(base_server_control_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	hotshutdown/0,hotshutdown/1,hotshutdown/2,
	openthedoor/0,closethedoor/0,open_gmdoor/0,
	cancel_shutdowncmd/0,
	backup_db/0,
	gen_data/0,
	clear_goals_data/0,
	write_flag_file/0,
	clear_flag_file/0,
	recovery_db/0,
	update_code/0,
	update_data/0,
	update_option/0,
	format_data/1
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_server_shared.hrl").

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------
%%% put(key, value)、get(key)在进程字典中存储和检索键值对
%%% 进程字典是一个进程独有的、存储键值对的数据结构
%% --------------------------------------------------------------------
start_link()->
	?base_gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

hotshutdown()->
	clear_flag_file(),
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	%%LineNode = lists:last(base_node_util:get_linenodes()),
	base_rpc_util:cast(CenterNode,?MODULE,{hotshutdown_start,600}),
	c:q().

hotshutdown(Time_s)->
	%%io:format("hotshutdown ~p ~n",[Time_s]),
	% LineNode = lists:last(base_node_util:get_linenodes()),
	LineNode = base_node_util:get_linenode(),
	base_rpc_util:cast(LineNode,?MODULE,{hotshutdown_start,Time_s}).

hotshutdown(Time_s,Reason)->
	%%io:format("hotshutdown ~p ~p ~n",[Time_s,Reason]),
	% LineNode = lists:last(base_node_util:get_linenodes()),
	LineNode = base_node_util:get_linenode(),
	if
		Reason =:= [] ->
			base_rpc_util:cast(LineNode,?MODULE,{hotshutdown_start,Time_s});
		true->
			%%io:format("hotshutdown ~p ~p ~n",[Time_s,Reason]),
			base_rpc_util:cast(LineNode,?MODULE,{hotshutdown_start,Time_s,Reason})
	end.

backup_db()->
	clear_flag_file(),
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_ping_util:wait_node_connect(db),
	DbNode = base_node_util:get_dbnode(),
	% DbNode = lists:last(base_node_util:get_dbnodes()),
	%%base_ping_util:ping(DbNode),
	base_rpc_util:cast({base_db_master_server,DbNode},{backupdata}),
	c:q().

recovery_db()->
	clear_flag_file(),
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_ping_util:wait_node_connect(db),
	DbNode = base_node_util:get_dbnode(),
	% DbNode = lists:last(base_node_util:get_dbnodes()),
	%%base_ping_util:ping(DbNode),
	base_rpc_util:cast({base_db_master_server,DbNode},{recoverydata}),
	c:q().

gen_data()->
	clear_flag_file(),
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_ping_util:wait_node_connect(db),
	DbNode = base_node_util:get_dbnode(),
	% DbNode = lists:last(base_node_util:get_dbnodes()),
	%%base_ping_util:ping(DbNode),
	base_rpc_util:cast({base_db_master_server,DbNode},{gen_data}),
	c:q().

clear_goals_data()->
	clear_flag_file(),
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_ping_util:wait_node_connect(db),
	DbNode = base_node_util:get_dbnode(),
	% DbNode = lists:last(base_node_util:get_dbnodes()),
	base_rpc_util:cast({base_db_master_server,DbNode},{clear_goals_data}),
	c:q().

create_giftcard()->
	clear_flag_file(),
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_ping_util:wait_node_connect(db),
	DbNode = base_node_util:get_dbnode(),
	% DbNode = lists:last(base_node_util:get_dbnodes()),
	%%base_ping_util:ping(DbNode),
	base_rpc_util:cast({base_db_master_server,DbNode},{create_giftcard}),
	c:q().

write_flag_file()->
	File = "update_server_flag",
	case file:open(File, [write]) of
		{ok,F}->
			file:close(F);
		_->
			base_logger_util:info_msg("can not open file ~p ~n",[File])
	end.

clear_flag_file()->
	File = "update_server_flag",
	file:delete(File).

update_code()->
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_rpc_util:cast(CenterNode,?MODULE,{update_code}),
	c:q().

update_data()->
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_rpc_util:cast(CenterNode,?MODULE,{update_data}),
	c:q().

update_option()->
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_rpc_util:cast(CenterNode,?MODULE,{update_option}),
	c:q().	

openthedoor()->
	GateNodeList = base_node_util:get_gatenodes(),
	lists:foreach(
			fun(GateNode)->
				base_rpc_util:asyn_call(GateNode,base_tcp_listener_server,enable_connect,[])
			end,
	GateNodeList).

closethedoor()->
	GateNodeList = base_node_util:get_gatenodes(),
	lists:foreach(
			fun(GateNode)->
				base_rpc_util:asyn_call(GateNode,base_tcp_listener_server,disable_connect,[])
			end,
	GateNodeList).

open_gmdoor()->
	GmNodes = base_node_util:get_gmnodes(),
	lists:foreach(
			fun(GmNode)->
				base_rpc_util:asyn_call(GmNode,gm_listener,enable_connect,[])
			end,
	GmNodes).

cancel_shutdowncmd()->
	% LineNode = lists:last(base_node_util:get_linenodes()),
	LineNode = base_node_util:get_linenode(),
	base_rpc_util:cast(LineNode,?MODULE,{cancelshutdowncmd}).


format_data(Param)->
	clear_flag_file(),
	[CenterNode|_] = base_node_util:get_argument('-line'),
	base_ping_util:ping(CenterNode),
	base_ping_util:wait_node_connect(db),
	DbNode = base_node_util:get_dbnode(),
	% DbNode = lists:last(base_node_util:get_dbnodes()),
	%%base_ping_util:ping(DbNode),
	base_rpc_util:cast({dbmaster,DbNode},{format_data,Param}),
	c:q().

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init(Args) ->
	put(last_shutdowncmd_id,0),
	put(shutdowncmd_flag,false),
	{ok, #state{}}.

?handle_call({query_time}, _From, State) ->
   Reply = os:timestamp(),
   {reply, Reply, State};
?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info({hotshutdown_start,Time_s}, State) ->
	case get(shutdowncmd_flag) of
		true->
			io:format("please cancel last cmd first ~n");
		Other->
			CmdId = get(last_shutdowncmd_id) + 1,
			put(last_shutdowncmd_id,CmdId),		
			put(shutdownreason,[]), 	
			put(shutdowncmd_flag,true),
			hotshutdown_server(CmdId,Time_s)
	end,
	{noreply, State};
?handle_info({manage_hotshutdown_start,Time_s,FromProc}, State) ->
	case get(shutdowncmd_flag) of
		true->
			base_logger_util:info_msg("please cancel last cmd first ~n");
		_Other->
			CmdId = get(last_shutdowncmd_id) + 1,
			put(last_shutdowncmd_id,CmdId),		
			put(shutdownreason,[]), 	
			put(shutdowncmd_flag,true),
			manage_hotshutdown_server(CmdId,Time_s,FromProc)
	end,
	{noreply,State};
?handle_info({hotshutdown_start,Time_s,ShutDownReason}, State) ->
	case get(shutdowncmd_flag) of
		true->
			io:format("please cancel last cmd first ~n");
		Other->
			CmdId = get(last_shutdowncmd_id) + 1,
			put(last_shutdowncmd_id,CmdId),		
			put(shutdownreason,ShutDownReason), 	
			put(shutdowncmd_flag,true),
			hotshutdown_server(CmdId,Time_s)
	end,
	{noreply, State};
?handle_info({hotshutdown, {CmdID,Time_s}}, State) ->
	hotshutdown_server(CmdID,Time_s),
	{noreply, State};
?handle_info({manage_hotshutdown,{CmdID,Time_s,FromProc}},State)->
	manage_hotshutdown_server(CmdID,Time_s,FromProc),
	{noreply,State};
?handle_info({cancelshutdowncmd},State)->
	cancel_lastshutdowncmd(),
	{noreply, State};
?handle_info({update_code},State)->
	handle_update_code(),
	{noreply, State};
?handle_info({update_data},State)->
	handle_update_data(),
	{noreply, State};
?handle_info({update_option},State)->
	handle_update_option(),
	{noreply, State};
?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
%%hot shutdown server at Time_s sec. later
hotshutdown_server(CmdID,Time_s)->
	case (CmdID =:= get(last_shutdowncmd_id)) and (get(shutdowncmd_flag)) of
		true->
			%%io:format("hotshutdown_server ~p ~n",[Time_s]),
			if
				Time_s > 15*60 ->
					LastTime_s = 15*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 15*60) or (Time_s > 10*60) ->
					LastTime_s = 10*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 10*60) or (Time_s > 5*60) ->
					LastTime_s = 5*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 5*60)  or (Time_s > 4*60) ->
					LastTime_s = 4*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 4*60)  or (Time_s > 3*60) ->
					LastTime_s = 3*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 3*60)  or (Time_s > 2*60) ->
					LastTime_s = 2*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 2*60)  or (Time_s > 60) ->
					LastTime_s = 60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 60)  or (Time_s > 30) ->
					LastTime_s = 30,
					NextTime_s = Time_s - LastTime_s;	
				(Time_s =:= 30)  or (Time_s > 10) ->
					LastTime_s = 10,
					NextTime_s = Time_s - LastTime_s;
				Time_s > 0 ->
					LastTime_s = Time_s - 1,
					NextTime_s = 1;		
				Time_s =:= 0 ->
					LastTime_s = 0,
					NextTime_s = 0,
					shutdown_server(),
					cancel_lastshutdowncmd()
			end,
	
			case Time_s > 0 of
				true->
					erlang:send_after(NextTime_s*1000,self(),{hotshutdown,{CmdID,LastTime_s}}),	
					case 	Time_s >= 10 of
						true->
							waring_broadcast(Time_s)	;
						false->
							countdown_broadcast(Time_s)
					end;
				_->
					nothing
			end;
		false->
			io:format("cancel shutdown cmd id ~p ~n",[CmdID])
	end.
	
manage_hotshutdown_server(CmdID,Time_s,FromProc)->
	case (CmdID =:= get(last_shutdowncmd_id)) and (get(shutdowncmd_flag)) of
		true->
			base_logger_util:info_msg("hotshutdown_server ~p ~n",[Time_s]),
			if
				Time_s > 15*60 ->
					LastTime_s = 15*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 15*60) or (Time_s > 10*60) ->
					LastTime_s = 10*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 10*60) or (Time_s > 5*60) ->
					LastTime_s = 5*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 5*60)  or (Time_s > 4*60) ->
					LastTime_s = 4*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 4*60)  or (Time_s > 3*60) ->
					LastTime_s = 3*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 3*60)  or (Time_s > 2*60) ->
					LastTime_s = 2*60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 2*60)  or (Time_s > 60) ->
					LastTime_s = 60,
					NextTime_s = Time_s - LastTime_s;
				(Time_s =:= 60)  or (Time_s > 30) ->
					LastTime_s = 30,
					NextTime_s = Time_s - LastTime_s;	
				(Time_s =:= 30)  or (Time_s > 10) ->
					LastTime_s = 10,
					NextTime_s = Time_s - LastTime_s;
				Time_s > 0 ->
					LastTime_s = Time_s - 1,
					NextTime_s = 1;		
				Time_s =:= 0 ->
					LastTime_s = 0,
					NextTime_s = 0,
					closethedoor(),
					kick_all_roles(),
					base_logger_util:info_msg("manage_hotshutdown ok fromnode:~p~n",[FromProc]),
					base_rpc_util:cast(FromProc,{hotshutdown_ok}),
					cancel_lastshutdowncmd()
			end,
			case Time_s > 0 of
				true->
					erlang:send_after(NextTime_s*1000,self(),{manage_hotshutdown,{CmdID,LastTime_s,FromProc}}),	
					case 	Time_s >= 10 of
						true->
							waring_broadcast(Time_s);
						false->
							countdown_broadcast(Time_s)
					end;
				_->
					nothing
			end;
		false->
			io:format("cancel shutdown cmd id ~p ~n",[CmdID])
	end.


kick_all_roles()->
	MapNode= base_node_util:get_mapnode(),
	base_rpc_util:asyn_call(MapNode,gm_order_op,kick_all,[]).

shutdown_server()->
	closethedoor(),
	kick_all_roles(),
	write_flag_file(),
	base_logger_util:info_msg("hot shutdown server complete!!!~n").

waring_broadcast(Time_s)->
	if
		Time_s > 60 ->
			BrdTime = integer_to_list(trunc(Time_s/60)) ++"分钟",
			BrdMsg = get(shutdownreason);
		true->
			BrdTime = integer_to_list(Time_s) ++"秒",
			BrdMsg = get(shutdownreason)
	end,

	if
		is_binary(BrdMsg)->
			NewBrdMsg = binary_to_list(BrdMsg);
		true->
			NewBrdMsg = BrdMsg
	end,
	MapNode = base_node_util:get_mapnode(),
	base_rpc_util:asyn_call(MapNode,system_chat_op,send_message,[23,[NewBrdMsg,BrdTime],[]]).
	
countdown_broadcast(Time_s)->
	BrdMsg = integer_to_list(Time_s) ++"秒",
	MapNode = base_node_util:get_mapnode(),
	base_rpc_util:asyn_call(MapNode,system_chat_op,send_message,[23,[[],BrdMsg],[]]).

cancel_lastshutdowncmd()->
	%%io:format("cancel_lastshutdowncmd ~n"),
	ResetCmdID = get(last_shutdowncmd_id)+1,
	put(last_shutdowncmd_id,ResetCmdID),		
	put(shutdowncmd_flag,false).

handle_update_code()->
	todo.
	% version_up:up_all().

handle_update_data()->
	todo.
	% version_up:up_data().

handle_update_option()->
	todo.
	% version_up:up_option().
