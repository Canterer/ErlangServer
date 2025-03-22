%% Description: TODO: DAL data access layer数据库访问层
-module(base_db_dal_util).
-include("base_define_shared.hrl").

-define(DAL_WRITE_RECORD,'ets_dal_write_record').
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0,set_write_flag/0,get_write_flag/0]).

-export([read_rpc/2,read/2,read_index_rpc/3,read_index/3,read_rpc/1,read/1,run_transaction_rpc/1,run_transaction/1,index_match_object_rpc/2,index_match_object/2]).
-export([write_rpc/1,write_rpc/4,write_rpc/5,write/1,write/4,write/5,async_write_rpc/1,async_write/1]).
-export([delete_rpc/2,delete/2,delete/4,delete_rpc/4,delete_object_rpc/1,delete_object/1]).
-export([clear_table/1,clear_table_rpc/1,delete_index_rpc/3,delete_index/3]).
-include_lib("stdlib/include/qlc.hrl").
%%
%% API Functions
%%
init()->
	try 
		?base_ets:new(?DAL_WRITE_RECORD, [set,named_table,public]) 
	catch 
		E:R-> 
			?base_logger_util:info_msg("init_dal_write_record Exception(~p:~p)~n", [E,R])
	end.

%%
%% Local Functions
%%
read_rpc(Table)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode->case base_rpc_util:asyn_call(DbNode, ?MODULE, read, [Table]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("read_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("read_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
					 {ok,Result}-> {ok,Result};
					 _Any-> {failed,"read_rpc Unknown error"}
				 end
	end.

read_rpc(Table,Key)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> 
				case base_rpc_util:asyn_call(DbNode, ?MODULE, read, [Table,Key]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("read_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("read_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
					 {ok,Result}-> {ok,Result};
					 _Any-> {failed,"read_rpc Unknown error"}
				 end
	end.

read_index_rpc(Table,SecondaryKey,Pos)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, read_index, [Table,SecondaryKey,Pos]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("read_index_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("read_index_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
					 {ok,Result}-> {ok,Result};
					 _Any-> {failed,"read_rpc Unknown error"}
				 end
	end.

run_transaction_rpc(Trascation)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, run_transaction, [Trascation]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("run_transaction_rpc error ~p ~n",[Reason]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("run_transaction_rpc error ~p ~n",[Reason]),{failed,Reason};
					 {ok,Result}-> {ok,Result};
					 _Any-> {failed,"read_rpc Unknown error"}
				 end
	end.

index_match_object_rpc(Pattern,Pos)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, index_match_object, [Pattern,Pos]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("index_match_object_rpc error ~p Pattern ~p ~n",[Reason,Pattern]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("index_match_object_rpc error ~p Pattern ~p ~n",[Reason,Pattern]),{failed,Reason};
					 {ok,Result}-> {ok,Result};
					 _Any-> {failed,"read_rpc Unknown error"}
				 end
	end.
  
index_match_object(Pattern,Pos)-> 
	?MNESIA_OPERATER("Pattern:~p,Pos:~p",[Pattern,Pos]),
	case catch mnesia:dirty_index_match_object(Pattern,Pos) of
		{'EXIT', Reason} ->
			?base_logger_util:info_msg("index_match_object error ~p Pattern ~p ~n",[Reason,Pattern]),{failed,Reason};
		Result when is_list(Result) -> {ok,Result};
		Result->
			?base_logger_util:info_msg("index_match_object error ~p Pattern ~p ~n",[Result,Pattern]),{failed,Result}
	end.

read(Table)->
	ReadFun = fun()-> qlc:e(qlc:q([X || X <- mnesia:table(Table)])) end,
	Res = case mnesia:transaction(ReadFun) of
		{aborted,Reason} -> ?base_logger_util:info_msg("read error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
		{atomic, []}	 -> {ok,[]};
		{atomic, Result}-> {ok,Result}
	end,
	% ?MNESIA_OPERATER_RESULT("(Table:~p) Result:~p",[Table,Res]),
	Res.

read(Table,Key)->
	Res = case catch  mnesia:dirty_read({Table,Key}) of
		{'EXIT',Reason} -> ?base_logger_util:info_msg("read error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
		Result when is_list(Result) -> {ok,Result};
		Result->
			?base_logger_util:info_msg("read error ~p ~n",[Result]),{failed,Result}
	end,
	?MNESIA_OPERATER_RESULT("(Table:~p,Key:~p) Result:~p",[Table,Key,Res]),
	Res.

read_index(Table,SecondaryKey,Pos)->
	Res = case catch  mnesia:dirty_index_read(Table, SecondaryKey, Pos) of
		{'EXIT',Reason} -> ?base_logger_util:info_msg("read_index error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
		Result when is_list(Result)-> {ok,Result};
		Result->
			?base_logger_util:info_msg("read_index error ~p ~n",[Result]),{failed,Result}
	end,
	?MNESIA_OPERATER_RESULT("(Table:~p,SecondaryKey:~p,Pos:~p) Result:~p",[Table,SecondaryKey,Pos,Res]),
	Res.

run_transaction(Transaction)->
	?MNESIA_OPERATER("Transaction:~p",[Transaction]),
	case mnesia:transaction(Transaction) of
		{aborted,Reason} -> ?base_logger_util:info_msg("run_transaction error ~p ~n",[Reason]),{failed,Reason};
		{atomic, []}	 -> {ok,[]};
		{atomic, Result}-> {ok,Result}
	end.

delete_rpc(Table,Key)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, delete, [Table,Key]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("delete_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("delete_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> {failed,"delete_rpc Unknown error"}
				 end
	end.

delete(Table,Key)->
	Res = case catch mnesia:dirty_delete({Table,Key}) of
		{'EXIT',Reason} -> {failed,Reason};
		ok	 -> {ok}
	end,
	?MNESIA_OPERATER_RESULT("(Table:~p,Key:~p) Result:~p",[Table,Key,Res]),
	Res.

delete_rpc(Table,TableKey,FieldIndex,FieldKey)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, delete, [Table,TableKey,FieldIndex,FieldKey]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("delete_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("delete_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> {failed,"delete_rpc Unknown error"}
				 end
	end.
	
delete(Table,TableKey,FieldIndex,FieldKey)->
	WriteFun = fun()->
					case mnesia:read(Table,TableKey) of
						[]-> failed;
						[Term]-> FieldValues = erlang:element(FieldIndex, Term),
								 case lists:keyfind(FieldKey, 1, FieldValues) of
									 false->
										 case lists:member(FieldKey, FieldValues) of
											 false-> ok;
											 _-> FieldValue = lists:delete(FieldKey, FieldValues),
												 Object = erlang:setelement(FieldIndex, Term, FieldValue),
												 mnesia:write(Object)
										 end;
									 _-> FieldValue = lists:keydelete(FieldKey, 1, FieldValues),
										 Object = erlang:setelement(FieldIndex, Term, FieldValue),
										 mnesia:write(Object)
								 end
					end
			   end,
	Res = case mnesia:transaction(WriteFun) of
		{aborted,Reason} -> ?base_logger_util:info_msg("delete_object error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
		{atomic, failed} -> ?base_logger_util:info_msg("delete_object Table ~p ~n",[Table]),{failed,"read table failed when write"};
		{atomic, ok}	 -> {ok}
	end,
	?MNESIA_OPERATER_RESULT("(Table:~p,TableKey:~p,FieldIndex:~p,FieldKey:~p) Result:~p",[Table,TableKey,FieldIndex,FieldKey,Res]),
	Res.

delete_object_rpc(Object)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, delete_object, [Object]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("delete_object error ~p Object ~p ~n",[Reason,Object]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("delete_object error ~p Object ~p ~n",[Reason,Object]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> {failed,"delete_object_rpc Unknown error"}
				 end
	end.

delete_object(Object)->
	Res = case catch mnesia:dirty_delete_object(Object) of
		{'EXIT',Reason} -> ?base_logger_util:info_msg("delete_object error ~p Object ~p ~n",[Reason,Object]),{failed,Reason};
		ok	 -> {ok}
	end,
	?MNESIA_OPERATER_RESULT("(Object:~p) Result:~p",[Object,Res]),
	Res.

delete_index_rpc(Table,SecondaryKey,Pos)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> 
			case base_rpc_util:asyn_call(DbNode, ?MODULE, delete_index, [Table,SecondaryKey,Pos]) of
				 {badrpc,Reason}-> ?base_logger_util:info_msg("delete_index_rpc error ~p~n",[Reason]),{failed,badrpc,Reason};
				 {failed,Reason}-> ?base_logger_util:info_msg("delete_index_rpc error ~p~n",[Reason]),{failed,Reason};
				 {ok}-> {ok};
				 _Any-> {failed,"delete_index_rpc Unknown error"}
			end
	end.
 
delete_index(Table,SecondaryKey,Pos)->
	case read_index(Table,SecondaryKey,Pos) of
		{ok,Results}->
			lists:foreach(fun(Object)-> base_db_dal_util:delete_object(Object) end, Results),{ok};
		{failed,Result}->
			?base_logger_util:info_msg("delete_index read_index error ~p ~n",[Result]),
			{failed,Result}
	end.

write_rpc(Object)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, write, [Object]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("write_rpc error ~p Object ~p ~n",[Reason,Object]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("write_rpc error ~p Object ~p ~n",[Reason,Object]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> ?base_logger_util:info_msg("write_rpc exception Object ~p ~n",[Object]),{failed,"write_rpc Unknown error"}
				 end
	end.
	
write_rpc(Table,TableKey,FieldIndex,Value)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, write, [Table,TableKey,FieldIndex,Value]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("write_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("write_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> {failed,"write_rpc Unknown error"}
				 end
	end.

write_rpc(Table,TableKey,FieldIndex,FieldKey,FieldTupleValue)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, write, [Table,TableKey,FieldIndex,FieldKey,FieldTupleValue]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("write_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("write_rpc error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> {failed,"write_rpc Unknown error"}
				 end
	end.



write(Object)->
	Res = case catch mnesia:dirty_write(Object) of
		{'EXIT',Reason} -> ?base_logger_util:info_msg("write error ~p Object ~p ~n",[Reason,Object]),{failed,Reason};
		ok	 -> {ok}
	end,
	?MNESIA_OPERATER_RESULT("(Object:~p) Result:~p",[Object,Res]),
	Res.

write(Table,TableKey,FieldIndex,Value)->
	WriteFun = fun()->
					case mnesia:read(Table,TableKey) of
						[]-> error;
						[Term]-> Object = erlang:setelement(FieldIndex, Term, Value),
								 mnesia:write(Object)
					end
			   end ,
	Res = case mnesia:transaction(WriteFun) of
		{aborted,Reason} -> ?base_logger_util:info_msg("write error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
		{atomic, failed} -> ?base_logger_util:info_msg("write error Table ~p ~n",[Table]),{failed,"read table failed when write"};
		{atomic, ok}	 -> {ok}
	end,
	?MNESIA_OPERATER_RESULT("(Table:~p,TableKey:~p,FieldIndex:~p,Value:~p) Result:~p",[Table,TableKey,FieldIndex,Value,Res]),
	Res.

write(Table,TableKey,FieldIndex,FieldKey,FieldTupleValue)->
	WriteFun = fun()->
					case mnesia:read(Table,TableKey) of
						[]-> failed;
						[Term]-> FieldValues = erlang:element(FieldIndex, Term),
								 NewFieldValue = case is_tuple(FieldTupleValue) of
												  true->
													  if erlang:element(1, FieldTupleValue) == FieldKey->
															 case lists:keyfind(FieldKey, 1, FieldValues) of
																 false-> FieldValues ++ [FieldTupleValue];
																 _-> lists:keyreplace(FieldKey, 1, FieldValues, FieldTupleValue)
															 end;
														 true->
															 case lists:member(FieldTupleValue, FieldValues) of
																 false-> FieldValues ++ [FieldTupleValue];
																 _-> FieldValues
															 end
													  end;
												  false->
													  case lists:member(FieldTupleValue, FieldValues) of
														  false-> FieldValues ++ [FieldTupleValue];
														  _-> FieldValues
													  end
											  end,			 
								 Object = erlang:setelement(FieldIndex, Term, NewFieldValue),
								 mnesia:write(Object)
					end
			   end ,
	Res = case mnesia:transaction(WriteFun) of
		{aborted,Reason} -> ?base_logger_util:info_msg("write error ~p Table ~p ~n",[Reason,Table]),{failed,Reason};
		{atomic, failed} -> ?base_logger_util:info_msg("write error Table ~p ~n",[Table]),{failed,"read table failed when write"};
		{atomic, ok}	 -> {ok}
	end,
	?MNESIA_OPERATER_RESULT("(Table:~p,TableKey:~p,FieldIndex:~p,FieldTupleValue:~p) Result:~p",[Table,TableKey,FieldIndex,FieldTupleValue,Res]),
	Res.
	
async_write_rpc(Object)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, async_write, [Object]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("async_write_rpc error ~p Object ~p ~n",[Reason,Object]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("async_write_rpc error ~p Object ~p ~n",[Reason,Object]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> {failed,"write_rpc Unknown error"}
				 end
	end.

async_write(Object)->
	?MNESIA_OPERATER("Object:~p",[Object]),
	try
		WriteFun = fun()->mnesia:write(Object)end,
		mnesia:activity(async_dirty,WriteFun),
		{ok}
	catch
		_E:Reason-> ?base_logger_util:info_msg("async_write error ~p Object ~p ~n",[Reason,Object]),{failed,Reason}
	end.

clear_table_rpc(Object)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, clear_table, [Object]) of
					 {badrpc,Reason}-> ?base_logger_util:info_msg("clear_table_rpc error ~p Object ~p ~n",[Reason,Object]),{failed,badrpc,Reason};
					 {failed,Reason}-> ?base_logger_util:info_msg("clear_table_rpc error ~p Object ~p ~n",[Reason,Object]),{failed,Reason};
					 {ok}-> {ok};
					 _Any-> {failed,"write_rpc Unknown error"}
				 end
	end.

clear_table(TableName)->
	?MNESIA_OPERATER("TableName ~p",[TableName]),
	case mnesia:clear_table(TableName) of
		{aborted,Reason} -> ?base_logger_util:info_msg("clear_table error ~p TableName ~p ~n",[Reason,Reason]),{failed,Reason};
		{atomic, ok}	 -> {ok}
	end.

set_write_flag()->
	?base_ets:insert(?DAL_WRITE_RECORD,{1,os:timestamp()}).

get_write_flag()->
	case ?base_ets:lookup(?DAL_WRITE_RECORD, 1) of
		[]-> undefined;
		[{_,Time}]->Time
	end.
