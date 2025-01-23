-module(base_node_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_all_nodes/0,get_nodes_without_hidden/0]).
-export([get_node_host/1,get_node_sname/1,get_node_sname_str/1]).
-export([get_allowable_nodes/1,get_node_procs/1,check_node_allowable/2]).
-export([get_dbnode/0,get_linenode/0,check_match_map_and_line/2]).
%%
%% API Functions
%%
get_all_nodes()->
	[node()| nodes()]++nodes(hidden).

get_nodes_without_hidden()->
	[node()| nodes()].

get_node_host(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[_NodeName,Host]-> Host;
		_-> []
	end.
get_node_sname(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName,_Host]-> list_to_atom(NodeName);
		_-> undefined
	end.
get_node_sname_str(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName,_Host]-> NodeName;
		_-> []
	end.

convert_sname_str(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName,_Host]-> NodeName;
		[NodeName]-> NodeName;
		_->[]
	end.

% 使用Fun检测列表中的元素，当检测某个元素为真时立刻返回序号
check_list(Fun, List) ->
	check_list(Fun, List, 1).
check_list(Fun, [H|T], Index) ->
	case Fun(H) of
		true -> Index;
		false-> check_list(Fun, T, Index+1)
	end;
check_list(Fun, [], Index) -> 0.

% 返回节点SName列表 对于该Proc被启用的节点列表
get_allowable_nodes(Key)->
	base_env_ets:get2(proc_allowed_nodes,Key,[]).
% 返回Proc列表,其中Proc被启用的节点列表中需包含该Node
get_node_procs(Node)->
	SNode = get_node_sname(Node),
	NodeInfos = base_env_ets:get(proc_allowed_nodes,[]),
	FilterProc = fun({_App,Nodes})->
				  lists:member(SNode, Nodes)
			 end,
	Infos = lists:filter(FilterProc, NodeInfos),
	lists:map(fun({Proc,_})-> Proc end, Infos).

check_node_allowable(Key, Node) ->
	% lists:member(get_node_sname(Node), get_allowable_nodes(Key)).
	NodeNameStr = get_node_sname_str(Node),
	check_list(
		fun(AllowNode) ->
			NodeNameStr =:= atom_to_list(AllowNode)
		end
		, get_allowable_nodes(Key)) > 0.

get_filter_nodes(Key)->
	SNodes = get_allowable_nodes(Key),
	AliveNodes = get_all_nodes(),
	lists:filter(
		fun(AliveNode)->
			SNodeName = get_node_sname(AliveNode),
			lists:member(SNodeName, SNodes)
		end, AliveNodes).

% 统一接口
% 仅存在一个db节点
get_dbnode()->
	[Node|_] = get_filter_nodes(db),
	Node.

% 仅存在一个line节点
get_linenode()->
	[Node|_] = get_filter_nodes(line),
	Node.

% LineId 与 Map节点名字一一对应  Map1节点名对应LineId为1
check_match_map_and_line(MapNode,LineId)->
	MapStr = "map"++erlang:integer_to_list(LineId),
	SNodeStr = get_node_sname_str(MapNode),
	string:str(SNodeStr, MapStr) > 0.

%%
%% Local Functions
%%

