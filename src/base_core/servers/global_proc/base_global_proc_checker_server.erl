-module(base_global_proc_checker_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(CHECK_FIRSTINTERVAL,1000).
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([is_ready/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).
-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
	gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

is_ready()->
	try
		gen_server:call(?SERVER, is_global_ready)
	catch
		_:_->false
	end.
%% ====================================================================
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
init([]) ->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	put(global_proc_ready,false),
	do_wait(),
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
handle_call(is_global_proc_ready, From, State) ->
	Reply = get(global_proc_ready),
	{reply, Reply, State};
handle_call(Request, From, State) ->
	Reply = ok,
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
handle_info( {check_global_proc},State)->
	 do_wait(),
	{noreply,State};

handle_info({stop}, State) ->
	{stop,normal,State};
 
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
	put(global_proc_ready,true),
	self() ! {stop},
	base_logger_util:msg("global_wait_proc finished ~n").

is_all_node_waite_finish(MyWaitList)->
	StillNotWaitedList = lists:filter(
		fun(ModuleName)-> 
			not base_global_proc_ets:is_global_proc_registed(ModuleName) 
		end, MyWaitList),
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
