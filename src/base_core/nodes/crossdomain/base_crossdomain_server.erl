%% Description: TODO: Add description to base_crossdomain_server
-module(base_crossdomain_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/0,
	send_file/1,
	add_port/1,
	del_port/1,
	make_normal_cross_file/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(DEFAULT_OPENPORT,[80,8080,8081,8082,8083,8084,8085]).
-define(CROSS_DOMAIN_TCP_OPTIONS, [
									binary, 
									{packet, 0}, % no packaging 
									{reuseaddr, true}, % allow rebind without waiting 
									{active, false},
									{exit_on_close, false}
								 ]).
-define(POLICY_PORT,843).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
% -record(state, {}).

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
start_link()->
	?base_gen_server:start_link({global,?SERVER}, ?MODULE, [], []).


send_file(CSock) ->
	gen_tcp:controlling_process(CSock, self()),
	case gen_tcp:recv(CSock, 0) of
		{ok, ?CROSS_DOMAIN_FLAG} -> 
			CrossFile = get_crossfile(),
			Data = CrossFile,
			gen_tcp:send(CSock, Data);
		_-> error
	end,
	gen_tcp:close(CSock).

add_port(PortList)->
	try
		global:send(?MODULE,{add_port,PortList})
	catch
		E:R->base_logger_util:info_msg("add port ~p error E:~p R:~p ~n",[PortList,E,R])
	end.

del_port(PortList)->
	try
		global:send(?MODULE,{del_port,PortList})
	catch
		E:R->base_logger_util:info_msg("del port ~p error E:~p R:~p ~n",[PortList,E,R])
	end.


    
make_normal_cross_file()->
	"<?xml version=\"1.0\"?>\n<!DOCTYPE cross-domain-policy SYSTEM "
	++"\"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">\n"
	++"<cross-domain-policy>\n"
    ++"<allow-access-from domain=\"*\" to-ports=\"*\"/>\n"
    ++"</cross-domain-policy>\n\0".


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init(Args) ->
	process_flag(trap_exit, true),
	OpenPortList = base_env_ets:get2(crossport,openport,?DEFAULT_OPENPORT),
	put(openport,OpenPortList),
	put(cross_file,make_cross_file()),
	start_server().
	% {ok, #state{}}.

?handle_call({get_crossfile}, _From, State) ->
    Reply = get(cross_file),
    {reply, Reply, State};
?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info({'EXIT', _Pid, normal}, State) ->
	{stop, normal, State};
?handle_info({'EXIT', _Pid, Reason}, State) ->
	case erlang:is_port(State) of
		true -> gen_tcp:close(State)
	end,
	case start_server() of
		{ok, LSock} -> {noreply, LSock};
		{stop, Reason} -> {stop, Reason, State};
		_->{stop,normal}
	end;
?handle_info({add_port,PortList},State)->
	add_port_rpc(PortList),
	{noreply, State};
?handle_info({del_port,PortList},State)->
	del_port_rpc(PortList),
	{noreply, State};
?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

start_server() ->
	% SName = node_util:get_match_snode(cross,node()),
	SName = base_node_util:get_node_sname(node()),
	PolicyPort = base_env_ets:get2(crossport, SName,?POLICY_PORT),
	case gen_tcp:listen(PolicyPort, ?CROSS_DOMAIN_TCP_OPTIONS) of
		{ok, LSock} ->
					  spawn_link(fun() -> loop(LSock) end),
					   %%io:format("listen port :~p~n",[PolicyPort]),
					  {ok, LSock};
		%% @todo throw exception here and we must do this at our work port 
		{error, Reason} ->%%io:format("listen error:~p~n",[Reason]), 
			{stop, Reason}
	end.

loop(LSock) ->
	case gen_tcp:accept(LSock) of
		{ok, CSock} ->%%io:format("socket connect ~p~n",[CSock]),
					  spawn(fun() -> send_file(CSock) end);
		{error, Reason} -> Reason
	end,
	loop(LSock).

get_crossfile()->
	try
		?base_gen_server:call({global,?MODULE},{get_crossfile})
	catch
		E:R->base_logger_util:info_msg("get_crossfile error E:~p R:~p ~n",[E,R]),
		[]
	end.

get_openportstr()->
	OpenPortList = get(openport),
	OpenPortStrList = lists:map(fun(Port)-> base_temp_util:make_int_str(Port) end,OpenPortList),
	string:join(OpenPortStrList,",").

make_cross_file()->
	"<?xml version=\"1.0\"?>\n<!DOCTYPE cross-domain-policy SYSTEM "
	++"\"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">\n"
	++"<cross-domain-policy>\n"
    ++"<allow-access-from domain=\"*\" to-ports=\""
	++get_openportstr()
	++"\"/>\n"
    ++"</cross-domain-policy>\n\0".


add_port_rpc(PortList)->
	%%OpenPortList = get(openport),
	lists:foreach(fun(Port)-> 
				case lists:member(Port,get(openport)) of
					true->
						nothing;
					_->
						put(openport,get(openport) ++ [Port])
				end		
					end,PortList),
	put(cross_file,make_cross_file()).
		

del_port_rpc(PortList)->
	lists:foreach(fun(Port)-> 
				case lists:member(Port,get(openport)) of
					true->
						put(openport,lists:delete(Port,get(openport)));
					_->
						nothing
				end		
					end,PortList),
	put(cross_file,make_cross_file()).
