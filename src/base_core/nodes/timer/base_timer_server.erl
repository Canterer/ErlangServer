%% Description: TODO: Add description to base_timer_server
-module(base_timer_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	query_time/0,
	start_at_app/0,
	start_at_process/0,
	get_correct_now/0,
	get_time_of_day/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
% deviation 偏离	
-define(DEVIATION_SECONDS,'$deviation_seconds$').
-define(SERVER_START_TIME,'$server_start_time$').
-define(DEVIATION_SECONDS_ETS,'$ets_deviation_seconds$').
-define(CENTER_FLASH_TIMEOUT, (60*1000*10)).


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
%% --------------------------------------------------------------------
start_link()->
	?base_gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

query_time()->
	?base_gen_server:call(?MODULE, {query_time}).

start_at_app()->
	Now = query_time_rpc(500),
	put_deviation_seconds(Now).

start_at_process()->
	Deviation = get_ets_deviation_seconds(),
	Time = get_ets_server_start_time(),
	put(?DEVIATION_SECONDS,Deviation),
	put(?SERVER_START_TIME,Time).
	
get_correct_now()->
	{A,B,C} = os:timestamp(),
	{A,B+get_deviation_seconds(),C}.

get_time_of_day()->
	{A,B,C} = os:timestamp(),
	{A2,B2,_} = get_server_start_time(),
	{A - A2,B - B2,C}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init(Args) ->
	{ok, #state{}}.

?handle_call({query_time}, _From, State) ->
   Reply = os:timestamp(),
   {reply, Reply, State};
?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
get_deviation_seconds()->
	case get(?DEVIATION_SECONDS) of
		undefined-> 0;
		V->V
	end.

get_server_start_time()->
	case get(?SERVER_START_TIME) of
		undefined-> {0,0,0};
		Time->Time
	end.

put_deviation_seconds(OtherTimer)->
	{A2,B2,_C2} = OtherTimer,
	{A1,B1,_C1} = os:timestamp(),
	Deviation = B2 + A2*1000000 - B1 - A1*1000000,
	?base_ets:new(?DEVIATION_SECONDS_ETS, [set,public,named_table]),
	?base_ets:insert(?DEVIATION_SECONDS_ETS, {1,Deviation}),
	?base_ets:insert(?DEVIATION_SECONDS_ETS, {2,{A1,B1,0}}).

get_ets_deviation_seconds()->
	case ?base_ets:lookup(?DEVIATION_SECONDS_ETS, 1) of
		[]-> 0;
		[{_,Deviation}]->Deviation
	end.

get_ets_server_start_time()->
	case ?base_ets:lookup(?DEVIATION_SECONDS_ETS, 2) of
		[]-> 0;
		[{_,Time}]->Time
	end.

query_time_rpc(N)->
	case N of
		0-> 0;
		_->
			case base_rpc_util:asyn_call(base_node_util:get_timernode(), ?MODULE, query_time, []) of
				{badrpc,Reason}-> base_logger_util:info_msg("query_timer error:~p~n",[Reason]),
								  timer:sleep(1000),
								  query_time_rpc(N-1);
				Deviation->	Deviation
			end
	end.