%% Description: TODO: Add description to base_auth_gm_processor_server
-module(base_auth_gm_processor_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	auth/6
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {auth_algorithm,secret,authtimeout}).

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
	base_gen_server:start_link({local,?SERVER},?MODULE,[],[]).

auth(FromNode,FromProc,GmUserName,GmUserId,Time,GmAuthResult)->
	base_global_proc_util:send(?SERVER, {auth_gm,{FromNode,FromProc,GmUserName, GmUserId,Time,GmAuthResult}}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_init([]) ->
	base_timer_server:start_at_process(),
	SecretKey =base_env_ets:get(gmsecretkey, ""),
	CfgTimeOut=base_env_ets:get(gmauthttimeout, 3600),
	AuthAlgorithmMod = base_env_ets:get(auth_gm_module,default_auth_gm_func_mod),
    {ok, #state{auth_algorithm=AuthAlgorithmMod,secret= SecretKey,authtimeout=CfgTimeOut}}.


do_handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

do_handle_cast(_Msg, State) ->
	{noreply, State}.

do_handle_info({auth_gm,{FromNode,FromProc,GmName, GmId,Time,AuthResult}},
	#state{
		auth_algorithm=Mod,
		secret=SecretKey,
		authtimeout=CfgTimeOut
	}=State) ->
	
	Fun = case SecretKey of
			""-> validate_gm_test;
			_->  validate_gm
	end,

	case Mod:Fun(GmName, GmId,Time,AuthResult,SecretKey,CfgTimeOut) of
		{ok,GmId}->
			base_logger_util:msg("~p login successed userid=~p~n",[GmName,GmId]),
			base_gm_client_fsm:auth_ok(FromNode, FromProc, GmId);
		{error, Reason}-> 
			base_logger_util:msg("~p login failed,Reason:~p ~n",[GmName, Reason]),
			base_gm_client_fsm:auth_failed(FromNode, FromProc, Reason)
	end,
    {noreply, State};
do_handle_info(_Info, State) ->
	{noreply, State}.

do_terminate(_Reason, _State) ->
	ok.

do_code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
