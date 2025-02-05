%% Description: TODO: Add description to base_application_server
-module(base_application_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start/1,start/2,
	force_start/0,
	wait_ets_init/0,
	wait_ets_init_fliter/1,
	sp_call/3
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(CHECK_RAM_INTERVAL,1000).
-define(ERLNULL,undefined).
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
start(Application)->
	force_start(),
	Cookie = base_env_ets:get(cookie,?ERLNULL),
	erlang:set_cookie(node(), Cookie),
	application:start(Application).

start(Application,Type)->
	force_start(),
	Cookie = base_env_ets:get(cookie,?ERLNULL),
	erlang:set_cookie(node(), Cookie),
	application:start(Application,Type).

force_start()->
	case erlang:whereis(?SERVER) of
		?ERLNULL->	base_gen_server:start_link({local,?SERVER},?MODULE, [], []);
		_->ignor
	end.

wait_ets_init()->
	base_gen_server:call(?SERVER, {wait_ets_init},infinity).

wait_ets_init_fliter(EtsFliter)->
	base_gen_server:call(?SERVER, {wait_ets_init_fliter,{EtsFliter}},infinity).

sp_call(M,F,A)->
	base_gen_server:call(?SERVER, {sp_call,M,F,A},infinity).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_init(_Args) ->
	filelib:ensure_dir("../log/"),
	FileName = "../log/"++atom_to_list(base_node_util:get_node_sname(node())) ++ "_node.log", 
	error_logger:logfile({open, FileName}),
	base_env_ets:init(),
	base_env_ets:reset(),
	base_global_proc_ets:init(),
	% version_up:init(),
	base_db_tools:wait_ets_create(),
	{ok, #state{}}.

do_handle_call({wait_ets_init}, _From, State) ->
	% env_ets:fresh(),
	base_db_tools:wait_ets_init(),
	Reply = ok,
	{reply, Reply, State};
do_handle_call({wait_ets_init_fliter,{EtsFliter}}, _From, State) ->
	% env_ets:fresh(),
	base_db_tools:wait_ets_init_fliter(EtsFliter),
	Reply = ok,
	{reply, Reply, State};
do_handle_call({sp_call,M,F,A},_From,State)->
	try
		apply(M,F,A)
	catch
		E:R-> base_logger_util:msg("~p : ~p~n",[E,R])
	end,
	Reply = ok,
	{reply, Reply, State};
do_handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

do_handle_cast(_Msg, State) ->
	{noreply, State}.

do_handle_info(_Info, State) ->
	{noreply, State}.

do_terminate(_Reason, _State) ->
	ok.

do_code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
