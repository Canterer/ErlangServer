-module(base_global_proc_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

wait_global_proc_register()->
	base_global_proc_sup:start_checker().
	%%wait_loop().

wait_loop()->
	case  base_global_proc_checker_server:is_ready() of
		true->
			nothing;
		false->
			timer:sleep(1000),
			wait_loop()			
	end.		


send(ModuleName,Msg)->
	case base_global_proc_ets:get_global_proc_node(ModuleName) of
		[]->
			base_logger_util:msg("base_global_proc_ets send ModuleName ~p  Msg ~p error not in node ~p !!! ~n",[ModuleName,Msg,node()]),
			error;
 			%%global:send(ModuleName,Msg);
		Node->
			base_rpc_util:cast(Node,ModuleName, Msg)
	end.

call(ModuleName,Msg)->
	call(ModuleName,Msg,5000).

call(ModuleName,Msg,TimeOut)->
	case base_global_proc_ets:get_global_proc_node(ModuleName) of
		[]->
			base_logger_util:msg("base_global_proc_ets send ModuleName ~p  Msg ~p error not in node ~p !!! ~n",[ModuleName,Msg,node()]),
			error;
 			%%gen_server:call({global,ModuleName},Msg,TimeOut);
		Node->
			gen_server:call({ModuleName,Node}, Msg, TimeOut)
	end.

