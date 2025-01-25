-module(base_rpc_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([asyn_call/4,cast/2,cast/3,mult_cast/3]).

%%
%% API Functions
%%
asyn_call(Node, NamedProc, Func, Args)->
	rpc:call(Node, NamedProc, Func, Args).

cast(NamedProc,Msg)->
	try
		NamedProc!Msg
	catch
		E:R->
			base_logger_util:msg("base_rpc_util cast NamedProc ~p Msg ~p ERROR ~p ~n",[NamedProc,Msg,erlang:get_stacktrace()]),
			error
	end.
cast(Node,NamedProc,Msg)->
	CurNode = node(),
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
	rpc:abcast(Nodes, NamedProc, Msg).
	
%%
%% Local Functions
%%

