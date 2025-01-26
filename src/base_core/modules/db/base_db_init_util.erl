-module(base_db_init_util).
-compile(export_all).
%%
%% Include files
%%
%%
%% Exported Functions
%%
-export([db_init_master/0,db_init_slave/0,create_split_table/3,db_init_line_master/0]). 

%%
%% API Functions
%%
%%

%% db_init_master、db_init_line_master分别用于管理disc数据、ram数据
%%
db_init_master()->
	base_logger_util:msg("~p:~p~n",[?MODULE,?LINE]),
	db_operater_behaviour:start(),
	base_logger_util:msg("~p:~p~n",[?MODULE,?LINE]),
	case mnesia:system_info(is_running) of
		yes ->	mnesia:stop();
		no -> o;
		starting -> mnesia:stop()
	end,
	mnesia:create_schema([node()]),
	db_init_disc_tables().

db_init_line_master()->
	db_operater_behaviour:start(),
	NeedShareNodes = 
	lists:filter(fun(Node)-> 
				base_db_tools:is_need_ram_table(Node)		 
		 end,base_node_util:get_all_nodes() ),
	lists:foreach(fun(Node)-> base_rpc_util:asyn_call(Node, mnesia, stop,[]) end, NeedShareNodes),
	mnesia:delete_schema(NeedShareNodes),
	mnesia:create_schema(NeedShareNodes),
	lists:foreach(fun(Node)-> base_rpc_util:asyn_call(Node, mnesia, start,[]) end, NeedShareNodes),
	db_init_ram_tables(),
	lists:foreach(fun(Node)->
				RamTables = base_db_tools:get_node_ram_tables(Node),
				lists:foreach(fun(Table)-> 
					TableNodes = mnesia:table_info(Table, ram_copies),
					case lists:member(Node, TableNodes) of
						false->
							mnesia:add_table_copy(Table,Node, ram_copies);
						true->
							ignor
				end end, RamTables)	
				end, NeedShareNodes).

db_init_slave()->
	DbNode = base_node_util:get_dbnode(),
	base_db_tools:config_disc_db_node(DbNode).

%%
%% Local Functions
%%
db_init_disc_tables()->
	mnesia:start(),
	db_operater_behaviour:create_all_disc_table().

db_init_ram_tables()->
	mnesia:start(),
	db_operater_behaviour:create_all_ram_table().

create_split_table(CreateMod,BaseTable,Table)->
	CreateMod:create_mnesia_split_table(BaseTable,Table).

