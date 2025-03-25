%% Description: TODO: Add description to socket_callback
-module(socket_callback).

%%
%% Include files
%%
%%
%% Exported Functions
%%
-export([
	get_client_mod/0,
	on_client_receive_packet/3,
	on_client_close_socket/2
]).

%%
%% API Functions
%%

get_client_mod()->
	[{?MODULE,on_client_receive_packet,[]},
	 {?MODULE,on_client_close_socket,[]}].

on_client_receive_packet(GateProc,Binary,RolePid)->
	base_logger_util:info_msg("~p:~p(GateProc:~p,RolePid:~p)~n",[?MODULE,?FUNCTION_NAME,GateProc,RolePid]),
	try
		<<ID:16, Binary1/binary>> = Binary,
		% 内部指定了转换规则 防止解码
		% Term = erlang:binary_to_term(Binary),
		% TempId = erlang:menemt(1,Term),
		% base_logger_util:info_msg("ID:~p TempId:~p~n",[ID,TempId]),
		% Message = erlang:menemt(2,Term),
		% ProtoName = login_pb:get_record_name(ID),
		% base_logger_util:info_msg("~p:~p(GateProc:~p,RolePid:~p,MsgId:~p,MsgName:~p)~n", [?MODULE,?FUNCTION_NAME,GateProc,RolePid,ID,ProtoName]),
		package_dispatcher:dispatch(ID,Binary1,GateProc,RolePid)
	catch
		_:_-> base_logger_util:info_msg("socket_callback:receive_packet parse error ~p~n stacktrace:~p~n",[Binary,erlang:get_stacktrace()])
	end.

on_client_close_socket(GateProc,RolePid) ->
	base_logger_util:info_msg("~p:~p(GateProc:~p,RolePid:~p)~n", [?MODULE,?FUNCTION_NAME,GateProc,RolePid]).
%%
%% Local Functions
%%

