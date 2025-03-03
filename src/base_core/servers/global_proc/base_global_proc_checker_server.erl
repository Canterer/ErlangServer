%% Description: TODO: Add description to base_global_proc_checker_server
%% 每个节点都具有一张base_global_proc_ets表，记录自身需要通信的全局进程列表
%% 同节点类型 gate1 gate2 拥有的全局进程表是一样的 global_proc_1只会属于一个节点 
-module(base_global_proc_checker_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	is_ready/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(CHECK_FIRSTINTERVAL,1000).

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
	base_gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

is_ready()->
	try
		base_gen_server:call(?SERVER, is_global_proc_ready)
	catch
		_:_->false
	end.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_init(_Args) ->
	put(global_proc_ready,false),
	do_wait(),
	{ok, #state{}}.

do_handle_call(is_global_proc_ready, _From, State) ->
	Reply = get(global_proc_ready),
	{reply, Reply, State};
do_handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

do_handle_cast(_Msg, State) ->
	{noreply, State}.

do_handle_info( {check_global_proc},State)->
	do_wait(),
	{noreply,State};
do_handle_info({stop}, State) ->
	{stop,normal,State};
do_handle_info(_Info, State) ->
	{noreply, State}.

do_terminate(_Reason, _State) ->
	ok.

do_code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
do_wait()->
	case base_env_ets:get(global_wait_proc,[]) of
		 []->
			wait_stop();
		 WaitList->
			 WaitAll = 
			 lists:foldl( fun({NodeKey,MyWaitList},Re)->
					if
						not Re->
							Re;
						true->
							% 限制只取本节点所对应的节点类型 需要通信的全局进程名列表
							case base_node_util:check_node_allowable(NodeKey, node()) of
								false->
									Re;
								true->
									is_all_node_waite_finish(MyWaitList)
							end
					end
				end,true,WaitList),
			 if
				 WaitAll->
					 wait_stop();
				 true->
					 erlang:send_after(?CHECK_FIRSTINTERVAL, self(), {check_global_proc})
			 end
	 end.


wait_stop()->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	put(global_proc_ready,true),
	self() ! {stop}.

% gate1节点会获取自身节点类型gate 对应的需要通信的全局进程名列表  gate=[global_proc1,global_proc2]
is_all_node_waite_finish(MyWaitList)->
	StillNotWaitedList = lists:filter(
		fun(ModuleName)-> 
			not base_global_proc_ets:is_global_proc_registed(ModuleName) 
		end, MyWaitList),
	base_logger_util:msg("~p:~p MyWaitList:~p StillNotWaitedList:~p~n",[?MODULE,?FUNCTION_NAME,MyWaitList,StillNotWaitedList]),
	if
		StillNotWaitedList=:=[]->
			true;			%%wait finish
		true->
			AllNodes = base_node_util:get_all_nodes(),
			base_logger_util:msg("TTTTT~p~n",[AllNodes]),
			lists:foreach(
				fun(ProcNotWaited)-> 
					wait_global_proc_regist(ProcNotWaited,AllNodes) 
				end, StillNotWaitedList),
			false
	end.

% 每个全局进程 只属于一个节点 即auction_manager只会在map1节点中启动
% map1、map2都是指定map_app运行逻辑,但map_app的启动流程会根据节点名选择性启动auction_manager
wait_global_proc_regist(ProcNotWaited,AllNodes)->
	MatchNodes = lists:filter(
		fun(CurNode)->
			base_node_util:check_node_allowable(ProcNotWaited, CurNode)				 
		end, AllNodes),
	case MatchNodes of
		[]->
			nothing;
		[MatchNode|_T]->
			base_global_proc_ets:regist_global_proc(ProcNotWaited,MatchNode)
	end.
