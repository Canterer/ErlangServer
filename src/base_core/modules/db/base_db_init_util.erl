-module(base_db_init_util).
%%
%% Include files
%%
-include("base_define_shared.hrl").
%%
%% Exported Functions
%%
-export([
	db_init_master/0,
	db_init_slave/0,
	create_split_table/3,
	db_init_line_master/0
]). 

%% db_init_master、db_init_line_master分别用于管理disc数据、ram数据
%%
db_init_master()->
	?ZS_LOG(),
	db_operater_behaviour:start(),
	?ZS_LOG(),
	case ?base_mnesia:system_info(is_running) of
		yes ->	?base_mnesia:stop();
		no -> o;
		starting -> ?base_mnesia:stop()
	end,
	?base_mnesia:create_schema([node()]),
	db_init_disc_tables().

db_init_line_master()->
	?ZS_LOG(),
	db_operater_behaviour:start(),
	NeedShareNodes = 
	lists:filter(fun(Node)-> 
				base_db_tools:is_need_ram_table(Node)		 
		 end,base_node_util:get_all_nodes() ),
	?ZS_LOG(),
	lists:foreach(fun(Node)-> base_rpc_util:asyn_call(Node, mnesia, stop,[]) end, NeedShareNodes),
	?base_mnesia:delete_schema(NeedShareNodes),
	?base_mnesia:create_schema(NeedShareNodes),
	lists:foreach(fun(Node)-> base_rpc_util:asyn_call(Node, mnesia, start,[]) end, NeedShareNodes),
	
	?ZS_LOG(),
	db_init_ram_tables(),
	?ZS_LOG("NeedShareNodes:~p",[NeedShareNodes]),
	lists:foreach(
		fun(Node)->
			RamTables = base_db_tools:get_node_ram_tables(Node),
			?ZS_LOG("RamTables:~p",[RamTables]),
			lists:foreach(
				fun(Table)-> 
					?ZS_LOG("Table:~p",[Table]),
					try
						TableNodes = ?base_mnesia:table_info(Table, ram_copies),
						?ZS_LOG("Table:~p TableNodes:~p",[Table,TableNodes]),
						case lists:member(Node, TableNodes) of
							false->
								?ZS_LOG(),
								?base_mnesia:add_table_copy(Table,Node, ram_copies);
							true->
								?ZS_LOG(),
								ignor
						end
					catch
						_:_-> 
							?base_logger_util:info_msg("~p:line:~p ?base_mnesia:table_info(Table:~p, ram_copies)~n",[?MODULE,?LINE,Table]),
							true
					end
				end, RamTables)	
		end, NeedShareNodes),
	?ZS_LOG().

db_init_slave()->
	?ZS_LOG(),
	DbNode = base_node_util:get_dbnode(),
	base_db_tools:config_disc_db_node(DbNode).

create_split_table(CreateMod,BaseTable,Table)->
	?ZSS(),
	?base_logger_util:info_msg("CreateMod:~p,BaseTable:~p,Table:~p",[CreateMod,BaseTable,Table]),
	?ZS(),
	?base_logger_util:info_msg("CreateMod:~p,BaseTable:~p,Table:~p",[CreateMod,BaseTable,Table]),
	?ZS("CreateMod:~p,BaseTable:~p,Table:~p",[CreateMod,BaseTable,Table]),
	CreateMod:create_mnesia_split_table(BaseTable,Table).

%%
%% Local Functions
%%
db_init_disc_tables()->
	?base_mnesia:start(),
	db_operater_behaviour:create_all_disc_table().

db_init_ram_tables()->
	?base_mnesia:start(),
	db_operater_behaviour:create_all_ram_table().
