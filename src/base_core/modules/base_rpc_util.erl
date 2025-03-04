-module(base_rpc_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
	asyn_call/4,
	asyn_call/5,
	cast/2,
	cast/3,
	mult_cast/3,
	is_process_alive/1,
	is_process_alive/2
]).

%%
%% API Functions
%%
asyn_call(Node, NamedProc, Func, Args)->
	base_logger_util:msg("~p:~p(Node:~p, NamedProc:~p, Func:~p, Args:~p)~n",[?MODULE,?FUNCTION_NAME,Node,NamedProc,Func,Args]),
	rpc:call(Node, NamedProc, Func, Args).

asyn_call(Node, NamedProc, Func, Args, Timeout)->
	base_logger_util:msg("~p:~p(Node:~p, NamedProc:~p, Func:~p, Args:~p, Timeout:~p)~n",[?MODULE,?FUNCTION_NAME,Node,NamedProc,Func,Args,Timeout]),
	rpc:call(Node, NamedProc, Func, Args, Timeout).

cast(NamedProc,Msg)->
	base_logger_util:msg("~p:~p(NamedProc:~p,Msg:~p)~n",[?MODULE,?FUNCTION_NAME,NamedProc,Msg]),
	try
		NamedProc!Msg
	catch
		E:R->
			base_logger_util:msg("base_rpc_util cast NamedProc ~p Msg ~p ERROR ~p ~n",[NamedProc,Msg,erlang:get_stacktrace()]),
			error
	end.
cast(Node,NamedProc,Msg)->
	CurNode = node(),
	base_logger_util:msg("~p:~p(Node:~p,NamedProc:~p,Msg:~p) CurNode:~p~n",[?MODULE,?FUNCTION_NAME,Node,NamedProc,Msg,CurNode]),
	try
		case Node of
			CurNode -> NamedProc ! Msg;
			_Node  ->  {NamedProc,Node} ! Msg%%rpc:abcast([Node],NamedProc, Msg) %% abcast 's first arg is NodeList
		end		
	catch 
		E:R ->
			base_logger_util:msg("base_rpc_util:cast exception[~p:~p]!Node ~p NamedProc ~p Message ~p ~n~p ~n",
				[E,R,Node,NamedProc,Msg,erlang:get_stacktrace()]),
			error
	end.

mult_cast(Nodes,NamedProc,Msg) ->
	base_logger_util:msg("~p:~p(Nodes:~p,NamedProc:~p,Msg:~p) CurNode:~p~n",[?MODULE,?FUNCTION_NAME,Nodes,NamedProc,Msg]),
	rpc:abcast(Nodes, NamedProc, Msg).

is_process_alive(Pid) when is_pid(Pid) ->
	rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

is_process_alive(undefined, _ProcName) ->
	false;
is_process_alive(_Node, undefined) ->
	false;
is_process_alive(Node, Pid) when is_pid(Pid) ->
	case rpc:call(Node, erlang, is_process_alive, [Pid]) of
		undefined ->
			false;
		_Pid ->
			true
	end;
is_process_alive(Node, ProcName) ->
	case rpc:call(Node, erlang, whereis, [ProcName]) of
		undefined ->
			false;
		_Pid ->
			true
	end.
%%
%% Local Functions
%%

