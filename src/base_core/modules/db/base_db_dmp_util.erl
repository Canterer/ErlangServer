%% Description: TODO: Add description to base_dp_dmp_util
-module(base_db_dmp_util).
-include("base_define_min.hrl").


-define(DATA_MODIFY_RECORDS_0,'$data_for_modify_record_0$').
-define(DATA_MODIFY_RECORDS_1,'$data_for_modify_record_1$').

-define(OPTION_SWAP_SECONDS,ets_swap_seconds).
-define(ETS_SWAP_SECONDS_DEFAULT,300).
%%
%% Exported Functions
%%
-export([
	async_write/2,
	async_write/5,
	async_write/6,
	async_delete/3,
	sync_write/2,
	sync_write/5,
	sync_write/6,
	sync_delete/3,
	flush_bundle/1,
	flush_data_batch/1,
	flush_data/1,
	flush_not_using/0,
	flush_all/0
]).

-export([init/0,get_using_ets/0,get_noinuse_ets/0]).


%%
%% API Functions
%%


init()->
	try 
		?base_ets:new(?DATA_MODIFY_RECORDS_0, [set,named_table,public]) 
	catch 
		E:R-> 
			?base_logger_util:info_msg("init_local_modify Exception(~p:~p)~n", [E,R])
	end,
	try 
		?base_ets:new(?DATA_MODIFY_RECORDS_1, [set,named_table,public]) 
	catch 
		E2:R2-> 
			?base_logger_util:info_msg("init_local_modify Exception(~p:~p)~n", [E2,R2])
	end.

write(BundleId,Object)->
	Table = erlang:element(1, Object),
	Key =  erlang:element(2, Object),
	EtsName = get_using_ets(),
	NotUseEtsName = get_noinuse_ets(),
	clear_deletekey(EtsName,NotUseEtsName,BundleId,Table,Key),
	?base_ets:insert(EtsName, {{w,BundleId,Table,Key},BundleId,Object}).

write(BundleId,Table,TableKey,FieldIndex,FieldValue)->
	EtsName = get_using_ets(),
	?base_ets:insert(EtsName, {{wf,BundleId,Table,TableKey,FieldIndex},BundleId,FieldValue}).

write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldTupleValue)->
	EtsName = get_using_ets(),
	?base_ets:insert(EtsName, {{wfk,BundleId,Table,TableKey,FieldIndex,FieldKey},BundleId,FieldTupleValue}).

delete(BundleId,Table,Key)->
	EtsName = get_using_ets(),
	NotUseEtsName = get_noinuse_ets(),
	clear_writekey(EtsName,NotUseEtsName,BundleId,Table,Key),
	?base_ets:insert(EtsName, {{del,BundleId,Table,Key},BundleId,Key}).

async_write(BundleId,Object)-> write(BundleId,Object).
async_write(BundleId,Table,TableKey,FieldIndex,FieldValue)->
	write(BundleId,Table,TableKey,FieldIndex,FieldValue).
async_write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldTupleValue)->
	write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldTupleValue).
async_delete(BundleId,Table,Key)-> delete(BundleId,Table,Key).


sync_write(BundleId,Object)->
	Table = erlang:element(1, Object),
	Key =  erlang:element(2, Object),
	EtsName = get_using_ets(),
	NotUseEtsName = get_noinuse_ets(),
	clear_deletekey(EtsName,NotUseEtsName,BundleId,Table,Key),	
	?base_ets:delete(EtsName,{w,BundleId,Table,Key}),
	?base_ets:delete(NotUseEtsName,{w,BundleId,Table,Key}),
	flush_data_rpc({{w,BundleId,Table,Key},BundleId,Object}).

sync_write(BundleId,Table,TableKey,FieldIndex,FieldValue)->
	EtsName = get_using_ets(),
	NotUseEtsName = get_noinuse_ets(),
	?base_ets:delete(EtsName,{wf,BundleId,Table,TableKey,FieldIndex}),
	?base_ets:delete(NotUseEtsName,{wf,BundleId,Table,TableKey,FieldIndex}),
	flush_data_rpc({{wf,BundleId,Table,TableKey,FieldIndex},BundleId,FieldValue}).

sync_write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldTupleValue)->
	EtsName = get_using_ets(),
	NotUseEtsName = get_noinuse_ets(),
	?base_ets:delete(EtsName,{wfk,BundleId,Table,TableKey,FieldIndex,FieldKey}),
	?base_ets:delete(NotUseEtsName,{wfk,BundleId,Table,TableKey,FieldIndex,FieldKey}),
	flush_data_rpc({{wfk,BundleId,Table,TableKey,FieldIndex,FieldKey},BundleId,FieldTupleValue}).

sync_delete(BundleId,Table,Key)->
	EtsName = get_using_ets(),
	NotUseEtsName = get_noinuse_ets(),
	clear_writekey(EtsName,NotUseEtsName,BundleId,Table,Key),
	?base_ets:delete(EtsName,{del,BundleId,Table,Key}),
	?base_ets:delete(NotUseEtsName,{del,BundleId,Table,Key}),
	flush_data_rpc({{del,BundleId,Table,Key},BundleId,Key}).

flush_not_using()->
	NotUseEtsName = get_noinuse_ets(),
	BundldObjects = ?base_ets:tab2list(NotUseEtsName),
	Len = length(BundldObjects),
	if
		Len >0-> ?base_logger_util:info_msg("flush_not_using size =~p~n",[Len]);
		true-> ignor
	end,
	?base_ets:delete_all_objects(NotUseEtsName),
	flush_data_rpc(BundldObjects).

flush_all()->
	NotUseEtsName = get_noinuse_ets(),
	BundldObjects1 = ?base_ets:tab2list(NotUseEtsName),
	?base_ets:delete_all_objects(NotUseEtsName),
	
	UseEtsName = get_using_ets(),
	BundldObjects2 = ?base_ets:tab2list(UseEtsName),
	?base_ets:delete_all_objects(UseEtsName),
	
	flush_data_rpc(BundldObjects1 ++ BundldObjects2).


flush_bundle(BundleId)->
	{Tab1,BundldObjects1} = get_using_bundle(BundleId),
	{Tab2,BundldObjects2} = get_notinuse_bundle(BundleId),
	lists:foreach(fun(X)-> ?base_ets:delete_object(Tab1, X) end, BundldObjects1),
	lists:foreach(fun(X)-> ?base_ets:delete_object(Tab2, X) end, BundldObjects2),
	flush_data_rpc(BundldObjects2 ++ BundldObjects1).

get_using_bundle(BundleId)->
	EtsName = get_using_ets(),
	BundldObjects = ?base_ets:match_object(EtsName, {'_',BundleId,'_'}),
	{EtsName,BundldObjects}.

get_notinuse_bundle(BundleId)->
	EtsName = get_noinuse_ets(),
	BundldObjects = ?base_ets:match_object(EtsName, {'_',BundleId,'_'}),
	{EtsName,BundldObjects}.

flush_data_rpc(DataOperates) when is_list(DataOperates)->
	base_rpc_util:asyn_call(base_node_util:get_dbnode(),?MODULE,flush_data_batch,[DataOperates]);
flush_data_rpc(DataOperate)->
	base_rpc_util:asyn_call(base_node_util:get_dbnode(),?MODULE,flush_data,[DataOperate]).

flush_data_batch(DataOperates)->
	lists:foreach(fun(DataOp)-> flush_data(DataOp) end, DataOperates).

flush_data({{w,_,_,_},_,Object})->
	base_db_dal_util:write(Object);
flush_data({{wf,_,Table,TableKey,FieldIndex},_,FieldValue})->
	base_db_dal_util:write(Table, TableKey, FieldIndex, FieldValue);
flush_data({{wfk,_,Table,TableKey,FieldIndex,FieldKey},_,FieldTupleValue})->
	base_db_dal_util:write(Table,TableKey,FieldIndex,FieldKey,FieldTupleValue);
flush_data({{del,_,Table,Key},_,_})->
	base_db_dal_util:delete(Table,Key).

%%
%% Local Functions
%%

clear_writekey(EtsName,NotUseEtsName,BundleId,Table,Key)->
	?base_ets:delete(EtsName,{w,BundleId,Table,Key}),
	?base_ets:delete(NotUseEtsName,{w,BundleId,Table,Key}).
clear_deletekey(EtsName,NotUseEtsName,BundleId,Table,Key)->
	?base_ets:delete(EtsName,{del,BundleId,Table,Key}),
	?base_ets:delete(NotUseEtsName,{del,BundleId,Table,Key}).
	

get_using_ets()->
	EtsSwapSeconds = base_env_ets:get2(dmp, ?OPTION_SWAP_SECONDS, ?ETS_SWAP_SECONDS_DEFAULT),
	{A,B,_C} = base_timer_server:get_correct_now(),
	NowSeconds = A*1000000 + B,
	get_using_ets_by_seconds(EtsSwapSeconds,NowSeconds).

get_noinuse_ets()->
	EtsSwapSeconds = base_env_ets:get2(dmp, ?OPTION_SWAP_SECONDS, ?ETS_SWAP_SECONDS_DEFAULT),
	{A,B,_C} = base_timer_server:get_correct_now(),
	NowSeconds = A*1000000 + B,
	get_notinuse_ets_seconds(EtsSwapSeconds,NowSeconds).
	
get_using_ets_by_seconds(SwapSeconds,NowSeconds)->
	case (NowSeconds div SwapSeconds) rem 2 of
		0-> ?DATA_MODIFY_RECORDS_0;
		1-> ?DATA_MODIFY_RECORDS_1
	end.

get_notinuse_ets_seconds(SwapSeconds,NowSeconds)->
	case (NowSeconds div SwapSeconds) rem 2 of
		0-> ?DATA_MODIFY_RECORDS_1;
		1-> ?DATA_MODIFY_RECORDS_0
	end.
