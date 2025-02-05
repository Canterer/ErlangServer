-module(base_global_proc_util).

%%
%% Include files
%%
-include("base_define_shared.hrl").
%%
%% Exported Functions
%%
-export([
	send/2,
	call/2,
	call/3
]).

-export([
	wait_global_proc_register/0
]).

wait_global_proc_register()->
	base_global_proc_sup:start_checker().
	%%wait_loop().

wait_loop()->
	?ZS_LOG(),
	case  base_global_proc_checker_server:is_ready() of
		true->
			nothing;
		false->
			timer:sleep(1000),
			wait_loop()			
	end.		


send(ModuleName,Msg)->
	?ZS_LOG(),
	case base_global_proc_ets:get_global_proc_node(ModuleName) of
		[]->
			base_logger_util:msg("base_global_proc_ets send ModuleName ~p  Msg ~p error not in node ~p !!! ~n",[ModuleName,Msg,node()]),
			error;
			%%global:send(ModuleName,Msg);
		Node->
			base_logger_util:msg("~p:~p(ModuleName:~p,Msg:~p) Node:~p~n",[?MODULE,?FUNCTION_NAME,ModuleName,Msg,Node]),
			base_rpc_util:cast(Node,ModuleName, Msg)
	end.

call(ModuleName,Msg)->
	call(ModuleName,Msg,5000).

call(ModuleName,Msg,TimeOut)->
	case base_global_proc_ets:get_global_proc_node(ModuleName) of
		[]->
			base_logger_util:msg("base_global_proc_ets send ModuleName ~p  Msg ~p error not in node ~p !!! ~n",[ModuleName,Msg,node()]),
			error;
			%%base_gen_server:call({global,ModuleName},Msg,TimeOut);
		Node->
			base_gen_server:call({ModuleName,Node}, Msg, TimeOut)
	end.

