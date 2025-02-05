%% Description: data modify processor of DAL
-module(base_db_dmp_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
%% double ets to save modify record 
-define(FLUSH_INTERVAL,1000*60*5).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {last_ets}).

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

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_init(_Args) ->
	?ZS_LOG(),
	base_timer_server:start_at_process(),
	FlushInterval = base_env_ets:get2(dmp, flush_interval, ?FLUSH_INTERVAL),
	base_db_dmp_util:init(),
	base_timer_util:send_after(FlushInterval, {flush_interval}),
	{ok, #state{last_ets=base_db_dmp_util:get_noinuse_ets()}}.

do_handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

do_handle_cast(_Msg, State) ->
	{noreply, State}.

do_handle_info({flush_interval},  #state{last_ets=LastEts} = State) ->
	CurEts = base_db_dmp_util:get_noinuse_ets(),
	if (LastEts =:= CurEts)->
					 base_db_dmp_util:flush_not_using();
				 true->
		   			 nothing
	end,
	FlushInterval = base_env_ets:get2(dmp, flush_interval, ?FLUSH_INTERVAL),
	base_timer_util:send_after(FlushInterval, {flush_interval}),
	{noreply,#state{last_ets=CurEts}};
do_handle_info(_Info, State) ->
	{noreply, State}.

do_terminate(_Reason, _State) ->
	ok.

do_code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
