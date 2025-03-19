%% Description: TODO: Add description to base_robot_client_tool
-module(base_robot_client_tool).
-define(BASE_MAX,200).
-define(MAPLIST,[100,200,300,500,600,700,1000,1300,1400,333]).
%% -define(MAPLIST,[100,200,300,400,500,600,800,900]).
%%
%% Include files
%%
-include("login_pb.hrl").
-include("base_robot_client_def.hrl").
%%
%% Exported Functions
%%
-export([
	start_robot_client_test/3,
	start_robot_client/8,
	start_robot_clients/4,
	start_robot_clients/6,
	start_robot_clients/7,
	start_robot_clients/8,
	start_robot_clients/9,
	call_robot/0
]).
-export([
	start_send/0,
	start_rec/0,
	test_send/0,
	test_rec/0
]).
-define(BASE,10000).
-define(DEFAULT_LINE_ID,1).
-define(DEFAULT_PORT,8001).
-define(DEFAULT_MAP_ID,100).
-define(DEFAULT_ROLE_LEVEL,40).
-define(DEFAULT_SPEEK_RATE,1).
%%
%% API Functions
%%

%%base_robot_client_tool:start_robot_client_test("127.0.0.1", 1, 1).
start_robot_client_test(Server,ServerId,Index)->
	io:format("start_robot_client_test(Server=~p,ServerId=~p,Index=~p)~n",[Server,ServerId,Index]),
	try
		% load_map_sup:start_link(),
		login_pb:create(),
		login_pb:init()
	catch
		_:_-> ignor
	end,
	filelib:ensure_dir("../log/"),
	FileName = "../log/"++atom_to_list(base_node_util:get_node_sname(node())) ++ "_node.log",
	error_logger:logfile({open, FileName}),
	start_robot_clients(Server,ServerId,Index,1).

start_robot_client(Server,ServerId,Index,LineId,Port,MapId,Level,SpeekRate)->
	Client_config = #robot_client_config{life_time=0,
					   user_name = 	Index,
				       user_password="123456",
				       server_addr= Server,
				       server_port= Port,
						lineid = LineId,
						mapid = MapId,
						level = Level,
						speekrate = SpeekRate,
						serverid = ServerId},
	base_logger_util:msg("start_client index:~p client_config:~p~n", [Index, Client_config]),
	% base_robot_client_app:start(list_to_atom(integer_to_list(Index)), Client_config).
	base_robot_client_fsm:start(list_to_atom(integer_to_list(Index)), Client_config).

%%base_robot_client_tool:start_robot_clients("192.168.1.251", 1, 10, 1).
start_robot_clients(Server,ServerId,Index,Num)->
	start_robot_clients(Server,ServerId,Index,Num,?DEFAULT_LINE_ID,?DEFAULT_PORT,?DEFAULT_MAP_ID,?DEFAULT_ROLE_LEVEL,?DEFAULT_SPEEK_RATE).

start_robot_clients(Server,ServerId,Index,Num,LineId,Port)->
	start_robot_clients(Server,ServerId,Index,Num,LineId,Port,?DEFAULT_MAP_ID,?DEFAULT_ROLE_LEVEL,?DEFAULT_SPEEK_RATE).

start_robot_clients(Server,ServerId,Index,Num,LineId,Port,MapId)->
	start_robot_clients(Server,ServerId,Index,Num,LineId,Port,MapId,?DEFAULT_ROLE_LEVEL,?DEFAULT_SPEEK_RATE).

start_robot_clients(Server,ServerId,Index,Num,LineId,Port,MapId,Level)->
	start_robot_clients(Server,ServerId,Index,Num,LineId,Port,MapId,Level,?DEFAULT_SPEEK_RATE).

start_robot_clients(Server,ServerId,Index,Num,LineId,Port,MapId,Level,SpeekRate)->
	RealIndex = Index * ?BASE_MAX + 1,
	try
		% load_map_sup:start_link(),
		login_pb:create(),
		login_pb:init()
	catch
		_:_-> ignor
	end,
	base_logger_util:msg("start_robot_clients Indexs:~p~n",[lists:seq(RealIndex,RealIndex + Num -1)]),
	lists:foreach(fun(IndexTmp)-> start_robot_client(Server,ServerId,IndexTmp,LineId,Port,MapId,Level,SpeekRate) end,lists:seq(RealIndex,RealIndex + Num -1)).


start_send()->
	register(test_send,spawn(?MODULE,test_send,[])).

test_send()->
	test_rec!{test_send,1}.
%% erlang:send_after(50,test_rec,{test_send,1}).

start_rec()->
	register(test_rec,spawn(?MODULE,test_rec,[])).

test_rec()->
	receive
		{test_send,N}->
			base_robot_client_tool:start_robot_client_test("192.168.1.251",N,1),
%% 			io:format("The N is=~p~n",[N]),
			if
				N<1000->
%% 					test_rec!{test_send,N+1};
					erlang:send_after(500,test_rec,{test_send,N+1});
				true->
					io:format("finish!~n")
			end;
		_->
			nothing
	end,
	test_rec().

call_robot()->
	base_robot_client_tool:start_robot_client_test("192.168.1.251",1,1).
%%
%% Local Functions
%%
