-module(base_global_proc_ets).
-include("base_define_min.hrl").

-define(GLOBAL_PROC_ETS,global_proc_ets).

%%
%% Exported Functions
%%

-export([init/0,is_global_proc_registed/1,regist_global_proc/2,get_global_proc_node/1,get_registed_global_procs/0]).

init()->
	try
		?base_ets:new(?GLOBAL_PROC_ETS, [named_table,public,set])
	catch
		E:R-> base_logger_util:info_msg("base_global_proc_ets create error ~p ~p ~n",[E,R])
	end.

get_registed_global_procs()->
	?base_ets:tab2list(?GLOBAL_PROC_ETS).

is_global_proc_registed(ModuleName)->
	case ?base_ets:lookup(?GLOBAL_PROC_ETS,ModuleName)  of
		[] ->
			false;
		[_] ->
			true
	end.

regist_global_proc(ModuleName,NodeName)->
	base_logger_util:info_msg("regist_global_proc ModuleName:~p NodeName:~p ~n",[ModuleName,NodeName]),
	?base_ets:insert(?GLOBAL_PROC_ETS,{ModuleName,NodeName}).

get_global_proc_node(ModuleName)->
	case get(proc_node_tuple_list) of
		undefined->
			case ?base_ets:lookup(?GLOBAL_PROC_ETS,ModuleName) of
				[]->
					base_logger_util:info_msg("ERROR global_proc Missed ModuleName ~p in ~p ~n",[ModuleName,node()]),
					case try_regist_proc(ModuleName) of
						[]->
							base_logger_util:info_msg("ERROR global error config module ~p in ~p ~n",[ModuleName,node()]),
							[];
						Node->
							put(proc_node_tuple_list,[{ModuleName,Node}|get(global_proc_node_map)])
					end;
				[{_,Node}]->
					Node
			end;
		ProcNodeInfoList->
			case lists:keyfind(ModuleName, 1, ProcNodeInfoList) of
				{_,Node}->
					Node;
				_->
					base_logger_util:info_msg("ERROR global Missed ModuleName ~p in ~p nodes ~p ~n",[ModuleName,node(),nodes()]),
					case try_regist_proc(ModuleName) of
						[]->
							base_logger_util:info_msg("ERROR global error config module ~p in ~p ~n",[ModuleName,node()]),
							[];
						Node->
							put(proc_node_tuple_list,[{ModuleName,Node}|get(proc_node_tuple_list)])
					end
			end
	end.

try_regist_proc(ModuleName)->
	AllNodes = base_node_util:get_all_nodes(),
	MatchNodes = lists:filter(fun(CurNode)->
		base_node_util:check_node_allowable(ModuleName, CurNode)				 
	end, AllNodes),
	case MatchNodes of
		[]->
			[];
		[MatchNode|_T]->
			regist_global_proc(ModuleName,MatchNode),
			MatchNode
	end.

