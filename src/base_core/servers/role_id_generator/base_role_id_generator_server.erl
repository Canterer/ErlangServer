%% Description: TODO: Add description to base_role_id_generator_server
-module(base_role_id_generator_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/1,
	gen_newid/1
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {servers_index}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_server_shared.hrl").
-include("common_define.hrl").

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------
%%% put(key, value)、get(key)在进程字典中存储和检索键值对
%%% 进程字典是一个进程独有的、存储键值对的数据结构
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
start_link(_ServerId)->
	?base_gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

gen_newid(ServerId)->
	base_global_proc_util:call(?MODULE, {gen_newid,ServerId}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init(Args) ->
	self()!{init_index},
	{ok, #state{servers_index = []}}.

?handle_call({gen_newid,ServerId}, _From, 
	#state{servers_index=ServersIndex}=_State) ->
	{ServerId,CurIndex} = lists:keyfind(ServerId,1, ServersIndex),
	
	NewCurIndx = case CurIndex of
		undefined -> base_gen_id_util:get_idmax({roleid,ServerId},?MIN_ROLE_ID) + 1;
		_-> CurIndex+1
	end,
	
	RoleId = ServerId*?SERVER_MAX_ROLE_NUMBER + NewCurIndx,
	%%
	Reply = RoleId,
%% 	Reply =  case ServerId of
%% 				0-> NewCurIndx;
%% 				_-> {ServerId,NewCurIndx}
%% 			  end,
	base_gen_id_util:update_idmax({roleid,ServerId},NewCurIndx),
	NewServersIndex = lists:keyreplace(ServerId, 1, ServersIndex,{ServerId,NewCurIndx}),
    {reply,Reply, #state{servers_index = NewServersIndex}};
?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info({init_index},State)->
	ServersIndex = 
	lists:map(fun(ServerId)->
		IdMax = base_gen_id_util:get_idmax({roleid,ServerId},?MIN_ROLE_ID),
		{ServerId,IdMax} end ,base_env_ets:get(serverids,[])),
    {noreply, State#state{servers_index=ServersIndex}};
?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
