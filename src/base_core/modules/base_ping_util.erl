%% Description: TODO: Add description to ping_center
-module(base_ping_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([wait_all_nodes_connect/0,ping/1,wait_node_connect/1]).

%%
%% API Functions
%%
wait_all_nodes_connect()->
	AllNodes = base_env_ets:get(pre_connect_nodes,[]),
	wait_nodes(AllNodes).

wait_node_connect(AppType)->
	AllNodes = base_env_ets:get(pre_connect_nodes,[]),
	NeedConNodes = lists:filter(
		fun(Node)-> 
			base_node_util:check_node_allowable(AppType,Node) 
		end, AllNodes),
	wait_nodes(NeedConNodes).
	
wait_nodes(AllNodes)->
	base_logger_util:msg("need wait nodes ~p ~n",[AllNodes]),
	lists:foreach(fun(Node)-> 
		base_logger_util:msg("ping Node ~p ~n",[Node]),				  
		ping(Node) end,AllNodes).			
	
ping(Node)->	
	ping_loop(Node).

ping_loop(Node)->
	case net_adm:ping(Node) of
		pong -> ok;
		_->
			receive 
			after
					1000 -> ping_loop(Node)
			end
	end.
		
	
%%
%% Local Functions
%%

