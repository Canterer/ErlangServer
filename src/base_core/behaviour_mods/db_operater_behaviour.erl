-module(db_operater_behaviour).

-define(DB_MOD_TABLE,dp_operater_mods).
-define(DB_SPLIT_TABLE,dp_split_tables).
-export([start/0]).
-export([start_module/2,init_ets/3]).
-export([create_all_disc_table/0,create_all_ram_table/0]).
-export([get_split_table_and_mod/1,get_all_split_table_and_mod/0]).
-export([get_all_ram_table/0]).
-export([get_backup_filter_tables/0]).
-export([behaviour_info/1]).
-export([delete_role_from_db/1]).

-include("base_define_shared.hrl").
%%
%%	behaviour fun
%%	copy this:		[start/0,create_mnesia_table/1,create_mnesia_split_table/2,delete_role_from_db/1]
%%

behaviour_info(callbacks) ->
	[
	{start,0},								%% start mod							%% args:[module,option]
	{create_mnesia_table,1},				%% create table not split 				%% args: ram/disc
	{create_mnesia_split_table,2},			%% create split table					%% args:[BaseTable,TrueTabName]
	{delete_role_from_db,1},				%% delete on role for persistent table  %% args:[roleid]
	{tables_info,0}							%% returns = [{DB,Type},...] Type:disc_split/disc/ram/proto
	];
behaviour_info(_Other) ->
	undefined.


%% @doc start gen_mod
start() ->
	?DB_OPERATER_START(),
	?DB_MOD_TABLE = ?base_ets:new(?DB_MOD_TABLE,
		[public, set, named_table, {keypos, 1}]),
	?DB_SPLIT_TABLE = ?base_ets:new(?DB_SPLIT_TABLE,
		[public, set, named_table, {keypos, 1}]),
	base_mod_util:behaviour_apply(db_operater_behaviour,start,[]),
	?DB_OPERATER_END(),
	ok.

start_module(Module, Opts)->
	TablesInfo = Module:tables_info(),
	lists:foreach(fun({Table,Type})-> 
				case Type of
					disc_split->
						true = ?base_ets:insert(?DB_SPLIT_TABLE, {Table,Module});
					_->
						nothing
				end end, TablesInfo),
	true = ?base_ets:insert(?DB_MOD_TABLE, {Module, Opts,TablesInfo}).

create_all_disc_table()->
	?DB_OPERATER_START(),
	?base_ets:foldl(fun({Module,_,_},_)->
				Module:create_mnesia_table(disc)	  
			end,[], ?DB_MOD_TABLE),
	?DB_OPERATER_END().

delete_role_from_db(RoleId)->
	?DB_OPERATER_START("RoleId:~p",[RoleId]),
	?base_ets:foldl(fun({Module,_,_},_)->
				Module:delete_role_from_db(RoleId)	  
			end,[], ?DB_MOD_TABLE),
	?DB_OPERATER_END().

create_all_ram_table()->
	?DB_OPERATER_START(),
	AllRamMod = ?base_ets:foldl(fun({Module,_,TablesInfo},AccMods)->
					case lists:keymember(ram,2,TablesInfo) of
						true->
							[Module|AccMods];
						_->
							AccMods
					end
			end,[], ?DB_MOD_TABLE),
	?ZS_LOG("AllRamMod:~p !!!",[AllRamMod]),
	lists:foreach(fun(Mod)->Mod:create_mnesia_table(ram) end,AllRamMod),
	?DB_OPERATER_END().

get_all_ram_table()->
	?base_ets:foldl(fun({_,_,TablesInfo},AccTables)->
					case lists:keyfind(ram,2,TablesInfo) of
						{Table,ram}->
							[Table|AccTables];
						_->
							AccTables
					end
			end,[], ?DB_MOD_TABLE).

get_split_table_and_mod(BaseTab)->
	case ?base_ets:lookup(?DB_SPLIT_TABLE, BaseTab) of
		[]->
			[];
		[Info]->
			Info
	end.
	
get_all_split_table_and_mod()->
	?ZS_LOG(),
	?base_ets:tab2list(?DB_SPLIT_TABLE).

get_backup_filter_tables()->
	?base_ets:foldl(fun({_,_,TablesInfo},AccMods)->
					lists:foldl(fun({TableName,TableType},AccTables)->
							case is_backup_filter_table({TableName,TableType}) of
								true->
									[TableName|AccTables];
								_->
									AccTables
							end end,[],TablesInfo) ++ AccMods 
			end,[], ?DB_MOD_TABLE).

is_backup_filter_table({_,ram})->
	true;
is_backup_filter_table({_,proto})->
	true;
is_backup_filter_table(_)->
	false.

%% args:
%% SourceDb: which db read from.
%% Ets: ets to write
%% EtsKeyPosOrPoses : [KeyPos]/[KeyPos1,KeyPos2,...]/KeyPos

init_ets(SourceDb,Ets,EtsKeyPosOrPoses) ->
	?DB_OPERATER_START("SourceDb:~p,Ets:~p,EtsKeyPosOrPoses:~p",[SourceDb,Ets,EtsKeyPosOrPoses]),
	?base_ets:delete_all_objects(Ets),
	case base_db_dal_util:read_rpc(SourceDb) of
		{ok,TermList} ->
			lists:foreach(fun(Term) -> 
					add_term_to_ets(Term,Ets,EtsKeyPosOrPoses)
					end,
					TermList);
		Error->
			?base_logger_util:info_msg("init_ets ~p failed from db ~p ~p ~p ~n",[Ets,SourceDb,Error])
	end,
	?DB_OPERATER_END().

add_term_to_ets(Term,Ets,KeyPoses)when is_list(KeyPoses)->
	Keyes = lists:map(fun(PosTmp)-> erlang:element(PosTmp,Term) end, KeyPoses),
	case KeyPoses of
		[KeyPos]->
			add_term_to_ets(Term,Ets,KeyPos);
		_->
			Key = erlang:list_to_tuple(Keyes),
			true  = ?base_ets:insert(Ets,{Key,Term})
	end;
add_term_to_ets(Term,Ets,KeyPos) ->
	Key = erlang:element(KeyPos,Term),
	true  = ?base_ets:insert(Ets,{Key,Term}).
