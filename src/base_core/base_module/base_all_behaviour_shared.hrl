%% Description: 本文件内含函数定义, include时需放在其他属性定义的最后
%% Description: 用于协调自定义behaviour函数导出与声明顺序
-ifndef(BEHAVIOUR_FUNC_START).
-include("base_define_shared.hrl").
-endif.
%% --------------------------------------------------------------------
%% behaviour functions exports
%% --------------------------------------------------------------------
-ifdef(ETS_OPERATER_BEHAVIOUR).
-behaviour(ets_operater_behaviour).
-export([
	create_ets/0,
	init_ets/0
]).
-endif.

-ifdef(DB_OPERATER_BEHAVIOUR).
-behaviour(db_operater_behaviour).
-export([
	start/0,
	create_mnesia_table/1,
	create_mnesia_split_table/2,
	delete_role_from_db/1,
	tables_info/0
]).
-endif.

-include("base_component_shared.hrl").
%% --------------------------------------------------------------------
%% behaviour functions
%% --------------------------------------------------------------------
-ifdef(ETS_OPERATER_BEHAVIOUR).
?log_create_ets()->
	?BEHAVIOUR_FUNC_START(),
	Returns = ?create_ets(),
	?BEHAVIOUR_FUNC_END("Returns=~p",[Returns]),
	Returns.

?log_init_ets()->
	?BEHAVIOUR_FUNC_START(),
	Returns = ?init_ets(),
	?BEHAVIOUR_FUNC_END("Returns=~p",[Returns]),
	Returns.
-endif.


-ifdef(DB_OPERATER_BEHAVIOUR).
?log_start()->
	?BEHAVIOUR_FUNC_START(),
	Returns = ?start(),
	?BEHAVIOUR_FUNC_END("Returns=~p",[Returns]),
	Returns.

% TableType: ram/disc
?log_create_mnesia_table(TableType)->
	?BEHAVIOUR_FUNC_START("TableType=~p", [TableType]),
	Returns = ?create_mnesia_table(TableType),
	?BEHAVIOUR_FUNC_END("Returns=~p",[Returns]),
	Returns.

?log_create_mnesia_split_table(BaseTable,TrueTabName)->
	?BEHAVIOUR_FUNC_START("BaseTable=~p, TrueTabName=~p", [BaseTable,TrueTabName]),
	Returns = ?create_mnesia_split_table(BaseTable,TrueTabName),
	?BEHAVIOUR_FUNC_END("Returns=~p",[Returns]),
	Returns.

?log_delete_role_from_db(RoleId)->
	?BEHAVIOUR_FUNC_START("RoleId=~p", [RoleId]),
	Returns = ?delete_role_from_db(RoleId),
	?BEHAVIOUR_FUNC_END("Returns=~p",[Returns]),
	Returns.

% returns = [{DB,Type},...]  Type:disc_split/disc/ram/proto
?log_tables_info()->
	?BEHAVIOUR_FUNC_START(),
	Returns = ?tables_info(),
	?BEHAVIOUR_FUNC_END("Returns=~p",[Returns]),
	Returns.
-endif.