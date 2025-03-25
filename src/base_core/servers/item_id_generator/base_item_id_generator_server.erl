%% Description: TODO: Add description to base_item_id_generator_server
-module(base_item_id_generator_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/1,
	gen_newid/0,
	test/1
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(MAX_NUMBER,999999).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {serverid,baseid,curindex}).

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
start_link(ServerId)->
	?base_gen_server:start_link({local,?SERVER}, ?MODULE, [ServerId], []).

gen_newid()->
	base_global_proc_util:call(?MODULE, {gen_newid}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([ServerId]) ->
	base_timer_server:start_at_process(),
	{BaseId,ContexBase} = get_base_id(ServerId),
    {ok, #state{serverid=ServerId,baseid={BaseId,ContexBase},curindex=1}}.

?handle_call({gen_newid}, _From, #state{serverid=ServerId,baseid={BaseId,ContexBase},curindex=CurIndex}=_State) ->
	Reply =  {BaseId,ContexBase+CurIndex},
	{BaseId2,ContexBase2,CurIndex2}=
									if
										CurIndex >= ?MAX_NUMBER ->
											?base_logger_util:info_msg("itemid have arrive ~p\nReset this processor\n",[CurIndex]),
											{NewBaseId,NewContexBase} = get_base_id(ServerId),
											{NewBaseId,NewContexBase,1};
										true -> {BaseId,ContexBase,CurIndex+1}
									end,
	
    {reply,Reply, #state{serverid=ServerId,baseid={BaseId2,ContexBase2},curindex=CurIndex2}};	
?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
get_base_id(ServerId)->
	Standard = {{2010,11,11},{0,0,0}},
	BaseSecond = calendar:datetime_to_gregorian_seconds(Standard),
	NowDate = calendar:now_to_local_time(base_timer_server:get_correct_now()),
	NowSecond = calendar:datetime_to_gregorian_seconds(NowDate),
	DiffSecond = NowSecond - BaseSecond,
	BaseId = (DiffSecond div 86400) * 1000000 + ServerId,
	CurIndx = (DiffSecond div 60) rem (1440),
	{BaseId,CurIndx*1000000}.
	
%%
%% For testing code 
%% TODO:change  MAX_NUMBER to 9999 & and test next code
%%
test(server)->
	start_link(999),
	lists:foreach(fun(_)-> spawn_link(fun()-> loop()end) end, lists:duplicate(1000, dummy));
test(client)->
	lists:foreach(fun(_)-> spawn_link(fun()-> loop()end) end, lists:duplicate(1000, dummy)).

loop()->
	gen_newid(),
	timer:sleep(1000),
	loop().
