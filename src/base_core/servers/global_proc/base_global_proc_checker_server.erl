%% Description: TODO: Add description to base_global_proc_checker_server
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
			lists:foreach(
				fun(ProcNotWaited)-> 
					wait_global_proc_regist(ProcNotWaited,AllNodes) 
				end, StillNotWaitedList),
			false
	end.

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
