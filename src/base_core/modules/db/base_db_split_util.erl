%% Description: TODO: Add description to db_split
-module(base_db_split_util).

%%
%% Include files
%%

-include("mnesia_table_def.hrl").
-include("common_define.hrl").
-include("base_define_shared.hrl").
-define(SPLIT_TABLE_NAME_ETS,tablename_by_splitted_ets).
-define(SPLIT_TABLE_MAX_NUM,500000).
%%
%% Exported Functions
%%
-compile(export_all).
-export([get_owner_table/2,check_need_split/1,get_table_names_rpc/1,get_table_names/1]).

-export([get_table_info/1]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
% -include("base_ets_operater_shared.hrl").
-define(ETS_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
%% --------------------------------------------------------------------
%%% behaviour functions begine
%% --------------------------------------------------------------------
do_create_ets()->
	ets_operater_behaviour:new(?SPLIT_TABLE_NAME_ETS,[set,public,named_table]).

do_init_ets()->
	nothing.
%% --------------------------------------------------------------------
%%% behaviour functions end
%% --------------------------------------------------------------------

%%
%% API Functions
%%
add_table_names(OriginalTable,SplittedTable)->
	case ets:lookup(?SPLIT_TABLE_NAME_ETS, OriginalTable) of
		[]->ets_operater_behaviour:insert(?SPLIT_TABLE_NAME_ETS, {OriginalTable,[SplittedTable]});
		[{_,OldTables}]->
			case lists:member(SplittedTable, OldTables) of
				true-> ignor;
				_->
					ets_operater_behaviour:insert(?SPLIT_TABLE_NAME_ETS, {OriginalTable,[SplittedTable|OldTables]})
			end
	end.

get_table_names(OriginalTable)->
	case ets:lookup(?SPLIT_TABLE_NAME_ETS, OriginalTable) of
		[]-> [];
		[{_,SplittedTables}]->SplittedTables
	end.

get_table_names_rpc(OriginalTable)->
	case base_node_util:get_dbnode() of
		nonode-> {nonode};
		DbNode-> case base_rpc_util:asyn_call(DbNode, ?MODULE, get_table_names, [OriginalTable]) of
					 {badrpc,Reason}-> {failed,badrpc,Reason};
					 {failed,Reason}-> {failed,Reason};
					 {ok,Result}-> {ok,Result};
					 _Any-> {failed,"get_table_names_rpc Unknown error"}
				 end
	end.
%%
%% Local Functions
%%
get_table_postfix(Id,Max)->
	OrgId = Id-?MIN_ROLE_ID,
	Hi = OrgId div ?SERVER_MAX_ROLE_NUMBER ,
	Low = OrgId rem ?SERVER_MAX_ROLE_NUMBER,
	TabIndx = Low div Max,
	erlang:integer_to_list(Hi) ++ "_" ++ erlang:integer_to_list(TabIndx).

get_owner_table(TableName,Id)->
	Max = 
	case TableName of
		playeritems->
			100000;
		pet_explore_storage->
			100000;
		role_treasure_storage->
			100000;
		_->	
			?SPLIT_TABLE_MAX_NUM
	end,
	NewTableString = erlang:atom_to_list(TableName) ++ "_" ++ get_table_postfix(Id,Max),
	erlang:list_to_atom(NewTableString).

check_need_split(HiValue)->
	SplitInfos = db_operater_behaviour:get_all_split_table_and_mod(),
	lists:foldl(fun(TabLine,Acc)->
						case Acc of
							ignor-> ignor;
							ok->check_need_new_table(get_basetable(TabLine),HiValue)
						end
				  end,ok,SplitInfos).

get_splitted_tables(TablseBase)->
	AllTables = mnesia:system_info(tables),
	lists:filter(fun(Table)-> check_tablebase_match(TablseBase,Table) end, AllTables).

check_tablebase_match(TableBase,CurBase)->
	TabStr = atom_to_list(TableBase),
	ExtrTabStr = atom_to_list(CurBase),
	case string:str(ExtrTabStr, TabStr) of
		0-> false;
		1-> true;
		_-> false
	end.


get_table_info(TableName)->
	db_operater_behaviour:get_split_table_and_mod(TableName).

get_basetable(TableInfo)->
	erlang:element(1, TableInfo).

get_table_mod(TableInfo)->
	erlang:element(2, TableInfo).

get_splitted_info(CurTable)->
	CurTableStr = atom_to_list(CurTable),
	SplitInfos = db_operater_behaviour:get_all_split_table_and_mod(),
	case lists:filter(fun(SplitInfo)->
						 TableBase = get_basetable(SplitInfo),
						 TableBaseStr = atom_to_list(TableBase),
						 case string:str(CurTableStr, TableBaseStr) of
							 1-> true;
							 _-> false
						 end
				 end, SplitInfos) of
		[SplitInfo1]->
			SplitInfo1;
		_-> false
	end.

%%return ok/error
create_split_table_by_name(Table)->
	case base_db_split_util:get_splitted_info(Table) of
		false-> 
			base_logger_util:msg("create_split_table_by_name error ! not split table! (~p)~n",[Table]),
			error;
		TableInfo->
			CreateMod = get_table_mod(TableInfo),
			BaseTable = get_basetable(TableInfo),
			base_db_init_util:create_split_table(CreateMod, BaseTable,Table),
			ok
	end.
		
check_split_table(TableBase)->
	db_operater_behaviour:get_split_table_and_mod(TableBase)=/=[].

check_need_new_table(BaseTable,HiValue)->
	Prefix = atom_to_list(BaseTable) ++ "_"++integer_to_list(HiValue)++"_",
	Tables = mnesia:system_info(tables),
	F = fun(TableName,Acc0)->
			case Acc0 of
				{ok,_LastTable}-> Acc0;
				{error}-> {error};
				{notable,LastMaxIndex}->
					case TableName of
						schema-> {notable,LastMaxIndex};
						_-> TableString = atom_to_list(TableName),
							case string:str(TableString,Prefix) of
								0-> {notable,LastMaxIndex};
								_Indx->
									Count = mnesia:table_info(TableName, size),
									case Count of
								   		0-> {ok,TableName};
										undefined->{error};
								   		_->  Int = erlang:length(Prefix),
											 PostNumString = string:sub_string(TableString, Int+1),
											 PostNum = erlang:list_to_integer(PostNumString),
									   		{notable,erlang:max(PostNum,LastMaxIndex)}
							   		  end
							end %% case string:str
					end %% case TableName
			end %% case Acc0
		end,
	case lists:foldl(F, {notable,-1}, Tables) of
		{ok,_TableName}-> ok;
		{error}-> ignor;
		{notable,MaxIndex}-> Table = make_table_name(BaseTable,HiValue,MaxIndex+1),
							 case get_table_info(BaseTable) of
								 []-> error;
								 TableInfo->
											 base_db_init_util:create_split_table(get_table_mod(TableInfo),BaseTable,Table),
											 ok
							 end
	end.


make_table_name(BaseTable,HiValue,LowValue)->
	TableString = atom_to_list(BaseTable) ++ "_"++integer_to_list(HiValue)++"_"++integer_to_list(LowValue),
	list_to_atom(TableString).

check_split_master_tables()->
	?ZS_LOG(),
	SplitInfos = db_operater_behaviour:get_all_split_table_and_mod(),
	base_logger_util:msg("~p:~p SplitInfos:~p~n",[?MODULE,?FUNCTION_NAME,SplitInfos]),
	TableBases = lists:map(fun(X)-> get_basetable(X) end, SplitInfos),
	base_logger_util:msg("~p:~p TableBases:~p~n",[?MODULE,?FUNCTION_NAME,TableBases]),
	lists:map(fun(TableBase)->
					  Tables = get_splitted_tables(TableBase),
					  lists:foreach(fun(Table)->
											add_table_names(TableBase,Table)
									end, Tables),
					  case Tables of
						  []-> false;
						  _->  true
					  end 
				  end, TableBases).


