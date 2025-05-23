%% Description: TODO: Add description to base_tcp_listener_server
-module(base_tcp_listener_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/4,
	disable_connect/0,
	enable_connect/0,
	query_port/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(ACCEPTOR_PROC_ETS,'$ets_acceptor_proc$').
-ifdef(debug).
	-define(OPEN_GATE_TIME,10*1000).		%%debug  time is short
	-define(OPEN_GATE_DOOR,enable_connect()).
-else.
	-define(OPEN_GATE_DOOR,disable_connect()).
	-define(OPEN_GATE_TIME,60*2*1000).	%%10min
-endif.

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {listen_socket, 	%% listen socket
				on_startup,	
				on_shutdown,
				acceptors}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_server_shared.hrl").
-include("network_setting.hrl").

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------
%%% put(key, value)、get(key)在进程字典中存储和检索键值对
%%% 进程字典是一个进程独有的、存储键值对的数据结构
%% --------------------------------------------------------------------
%% OnAccept : {M,F} ,M : Module; F: F(Pid,Socket)
%% ====================================================================
start_link(Port, AcceptorCount ,
		   OnStartup, OnShutdown) ->
	?base_gen_server:start_link({local, ?SERVER}, ?MODULE, 
						  {Port, AcceptorCount  ,
						   OnStartup, OnShutdown}, []).

query_port()->
	?base_gen_server:call(?SERVER, {query_port} ,infinity).

%%--------------------------------------------------------------------
%% Function: disable_connect/0
%% Description: disable gate connect
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
disable_connect()->
	%%gen_server:send({local, ?SERVER}, {disable_connect}).
%%	base_logger_util:info_msg("disable_connect ~p ~n",[?MODULE]),
	erlang:send_after(0,?SERVER,{disable_connect}).

%%--------------------------------------------------------------------
%% Function: enable_connnect/0
%% Description: enable gate connect
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
enable_connect()->
	%%gen_server:send({local, ?SERVER}, {enable_connect}).
	erlang:send_after(0,?SERVER,{enable_connect}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init({Port,AcceptorCount,{M,F,A} = OnStartup, OnShutdown}) ->
	base_logger_util:info_msg("~p:~p({Port:~p, AcceptorCount:~p, {M:~p,F:~p,A:~p}, OnShutdown:~p})~n",[?MODULE,?FUNCTION_NAME,Port,AcceptorCount,M,F,A,OnShutdown]),
	process_flag(trap_exit, true),
	Opts = ?TCP_OPTIONS,
	case gen_tcp:listen(Port, Opts) of
		{ok, Listen_socket} ->
			 SeqList = lists:seq(1, AcceptorCount),
			 Fun = fun(AccIndex,Acc)->
						   {ok, _APid} = supervisor:start_child(base_tcp_acceptor_sup, [Listen_socket,AccIndex]),
						   
						   AcceptorName = base_tcp_acceptor_server:get_proc_name(AccIndex),
%%@@ 						   ?OPEN_GATE_DOOR,
%%@@ 						   erlang:send_after(?OPEN_GATE_TIME,?MODULE,{enable_connect}),
                           erlang:send_after(0,?SERVER,{3}),
						   [AcceptorName|Acc]
				   end,
             AccProcs = lists:foldl(Fun,[],SeqList),
			apply(M,F,A ++ [Listen_socket,Port]),
			
			{ok, #state{listen_socket = Listen_socket,
                        on_startup = OnStartup, on_shutdown = OnShutdown,
						acceptors=AccProcs}};
	
		{error, Reason} ->
			{stop, Reason}
	end.

?handle_call(reset_opt, _From, #state{listen_socket = Listen_socket} = State) ->
	inet:setopts(Listen_socket,[{packet, 0}]),
	base_logger_util:info_msg("inet:getopts active,packet ~p ~n",[inet:getopts(Listen_socket, [active,packet])]),
    Reply = ok,
    {reply, Reply, State};
?handle_call({query_port}, _From, #state{listen_socket = LSock} = State) ->
	 {ok, {_IPAddress, Port}} = inet:sockname(LSock),
    Reply = Port,
    {reply, Reply, State};
?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info({disable_connect}, State = #state{acceptors=AccProcs}) ->
%%	base_logger_util:info_msg("handle_info disable_connect ~p ~n",[?MODULE]),
	lists:foreach(fun(AcceptorName)->
						  R = base_tcp_acceptor_server:disable_connect(AcceptorName),
						  base_logger_util:info_msg("Acceptor Stat:~p~n",[R])
				  end, AccProcs),
    {noreply, State};
?handle_info({enable_connect},State = #state{acceptors=AccProcs})->
	lists:foreach(fun(AcceptorName)->
						  R = base_tcp_acceptor_server:enable_connect(AcceptorName),
						  base_logger_util:info_msg("Acceptor Stat:~p~n",[R])
				  end, AccProcs),
	{noreply, State};
?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, #state{listen_socket=LSock, on_shutdown = {M,F,A}}) ->
    {ok, {_IPAddress, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
    apply(M, F, A ++ [LSock, Port]).

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
