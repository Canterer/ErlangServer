%% Description: TODO: Add description to autoname_db
-module(autoname_db).
-define(AUTONAME_ETS,autoname_table).
%%
%% Include files
%%
-include("mnesia_table_def.hrl").
%%
%% Exported Functions
%%
-export([
	get_autoname_info/1,
	sync_update_autoname_used_to_mnesia/1,
	get_autoname_used/1
]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
-define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
%% --------------------------------------------------------------------
%%% behaviour functions begine
%% --------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%?
?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	base_db_tools:create_table_disc(auto_name,record_info(fields,auto_name),[],set),
	base_db_tools:create_table_disc(auto_name_used,record_info(fields,auto_name_used),[],set).

?create_mnesia_split_table(_,_)->
	nothing.

?delete_role_from_db(_)->
	nothing.

?tables_info()->
	[{auto_name,proto},{auto_name_used,disc}].

?create_ets()->
	?base_ets:new(?AUTONAME_ETS,[set,public,named_table]).

?init_ets()->
	db_operater_behaviour:init_ets(auto_name, ?AUTONAME_ETS,#auto_name.id).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_autoname_info(Id)->
	case ?base_ets:lookup(?AUTONAME_ETS, Id) of
		[]->[];
        [{_,Info}]-> Info 
	end.

sync_update_autoname_used_to_mnesia(Term)->
	Object = base_temp_util:term_to_record(Term,auto_name_used),
	base_db_dal_util:write_rpc(Object).

get_autoname_used(RoleName)->
	case base_db_dal_util:read_rpc(auto_name_used,RoleName) of
		{ok,[]}-> {ok,[]};
		{ok,Result}-> {ok,Result};
		{failed,badrpc,Reason}-> ?base_logger_util:info_msg("get_autoname_used failed ~p:~p~n",[badrpc,Reason]);
		{failed,Reason}-> ?base_logger_util:info_msg("get_autoname_used failed :~p~n",[Reason])
	end.
%%
%% Local Functions
%%

