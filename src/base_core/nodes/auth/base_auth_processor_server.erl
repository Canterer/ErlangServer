%% Description: TODO: Add description to base_auth_processor_server
-module(base_auth_processor_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	auth/4
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(AUTH_FAILED,-1).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {auth_algorithm,visitor_key,authtimeout,fatigue_list,nofatigue_list}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_server_shared.hrl").
-include("user_auth.hrl").

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------
%%% put(key, value)、get(key)在进程字典中存储和检索键值对
%%% 进程字典是一个进程独有的、存储键值对的数据结构
%% --------------------------------------------------------------------
start_link()->
	?base_gen_server:start_link({local,?SERVER},?MODULE,[],[]).

auth(FromNode,FromProc,ServerId,UserAuth)->
	base_logger_util:info_msg("~p:~p(FromNode:~p,FromProc:~p,ServerId:~p,UserAuth:~p)~n",[?MODULE,?FUNCTION_NAME,FromNode,FromProc,ServerId,UserAuth]),
    base_global_proc_util:send(?SERVER, {auth_player,{FromNode,FromProc,ServerId,UserAuth}}).

%%is_visitor_c2s
%% auth(FromNode,FromProc,Time,AuthResult)->
%%     base_global_proc_util:send(?MODULE, {auth_player,{FromNode,FromProc,Time,AuthResult}}).

%% auth(FromNode,FromProc,Time,AuthResult,AccountName)->
%%     base_global_proc_util:send(?MODULE, {auth_player,{FromNode,FromProc,Time,AuthResult,AccountName}}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([]) ->
	base_timer_server:start_at_process(),
	CfgTimeOut=base_env_ets:get(authtimeout, 3600),
	FatigueList = base_env_ets:get2(fatigue, fatigue_list, []),
	NoFatigueList = base_env_ets:get2(fatigue, nofatigue_list, []),
	VisitorKey = base_env_ets:get(visitorkey,""),
	AuthAlgorithmMod = base_env_ets:get(auth_module,default_auth_func_mod),
	{ok, #state{
		auth_algorithm=AuthAlgorithmMod,
		visitor_key=VisitorKey,
		authtimeout=CfgTimeOut,
		fatigue_list=FatigueList,
		nofatigue_list=NoFatigueList}}.

?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

% ?handle_info({auth_player,{FromNode,FromProc,Time,AuthResult,AccountName}},
% 	#state{auth_algorithm=Mod,visitor_key=VisitorKey,authtimeout=CfgTimeOut}=State) ->
% 	Fun = case VisitorKey of
% 			""-> validate_user_test;
% 			_->  validate_visitor
% 	end,
% 	case Mod:Fun(Time,AuthResult,VisitorKey,CfgTimeOut,false) of
% 		{ok,{_PlayerId,_AccountName},IsAudult}->
% 			base_tcp_client_statem:auth_ok(FromNode, FromProc, {finish_visitor,AccountName},AccountName,IsAudult);
% 		{error, Reason}-> 
% 			base_logger_util:info_msg("vistor login failed,Reason:~p ~n",[Reason]),
% 			base_tcp_client_statem:auth_failed(FromNode, FromProc, Reason)
% 	end,
% 	{noreply, State};
% %is_visitor_c2s
% ?handle_info({auth_player,{FromNode,FromProc,Time,AuthResult}},
% 	#state{auth_algorithm=Mod,visitor_key=VisitorKey,authtimeout=CfgTimeOut}=State)->
% 	Fun = case VisitorKey of
% 			""-> validate_user_test;
% 			_->  validate_visitor
% 	end,
% 	case Mod:Fun(Time,AuthResult,VisitorKey,CfgTimeOut,true) of
% 		{ok,{PlayerId,PlayerName},IsAudult}->
% 			base_logger_util:info_msg("vistor login successed ~p ~p~n",[PlayerId,PlayerName]),
% 			base_tcp_client_statem:auth_ok(FromNode, FromProc, {visitor,PlayerId},PlayerName,IsAudult);
% 		{error, Reason}-> 
% 			base_logger_util:info_msg("vistor login failed,Reason:~p ~n",[Reason]),
% 			base_tcp_client_statem:auth_failed(FromNode, FromProc, Reason)
% 	end,
% 	{noreply, State};
%%user_auth_c2s
?handle_info({auth_player,{FromNode,FromProc,ServerId,UserAuth}},State) ->
	#state{auth_algorithm=Mod,authtimeout=CfgTimeOut,fatigue_list=FatigueList,nofatigue_list=NoFatigueList}=State,
	SecretKey = base_env_ets:get(platformkey, ""),
	Fun = case SecretKey of
			""-> validate_user_test;
			_->  validate_user
	end,
	#user_auth{username=UserName,userid=UserId,pf=Pf,lgtime=LogTime,userip=UserIp,openid=OpenId,openkey=OpenKey,pfkey=PfKey} = UserAuth,
	% base_logger_util:info_msg("auth_player userauth:~p~n,serverid ~p ~n",[UserAuth,ServerId]),
	% base_logger_util:info_msg("mod fun is ~p~n",[{Mod,Fun}]),
	try
		case Mod:Fun(UserAuth,SecretKey,CfgTimeOut,FatigueList,NoFatigueList) of
			{ok,PlayerId,IsAudult}->
				% base_logger_util:info_msg("~p login successed userid=~p~n",[UserName,PlayerId]),
				base_tcp_client_statem:auth_ok(FromNode, FromProc,ServerId,PlayerId,UserName,IsAudult);
			{ok,Info}->
				% base_logger_util:info_msg("qq_auth_ok login successed username=~p  userid=~p~n",[UserName,UserId]),
				base_tcp_client_statem:qq_auth_ok(FromNode,FromProc,ServerId,UserId,UserName,LogTime,Pf,UserIp,Info,OpenId,OpenKey,PfKey);
			{error, Reason}-> 
				base_logger_util:info_msg("~p login failed,Reason:~p ~n",[UserName, Reason]),
				base_tcp_client_statem:auth_failed(FromNode, FromProc, ServerId,Reason)
		end
	catch
		R:E->
			base_logger_util:info_msg("base_auth_processor_server error,R:~p,E:~p,UserAuth:~p~n",[R,E,UserAuth]),
			base_tcp_client_statem:auth_failed(FromNode, FromProc, ServerId,?AUTH_FAILED)
	end,
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
