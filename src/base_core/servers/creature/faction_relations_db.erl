-module(faction_relations_db).

-export([
	get_info/1,
	get_id/1,
	get_friendly/1,
	get_opposite/1
]).

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
-define(ETS_OPERATER_BEHAVIOUR,true).
-define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
-include("mnesia_table_def.hrl").
-define(FACTION_RELATIONS_TABLE_NAME,faction_relations_db).
%% --------------------------------------------------------------------
%%% behaviour functions begine
%% --------------------------------------------------------------------
?create_ets()->
	?base_ets:new(?FACTION_RELATIONS_TABLE_NAME, [set,named_table]).

?init_ets()->
	db_operater_behaviour:init_ets(faction_relations, ?FACTION_RELATIONS_TABLE_NAME,#faction_relations.id).
?start()->
	db_operater_behaviour:start_module(?MODULE,[]).

?create_mnesia_table(disc)->
	base_db_tools:create_table_disc(faction_relations,record_info(fields,faction_relations),[],set).

?create_mnesia_split_table(_,_)->
	nothing.

?tables_info()->
	[{faction_relations,proto}].

?delete_role_from_db(_RoleId)->
	nothing.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_info(FactionId)->
	case ?base_ets:lookup(?FACTION_RELATIONS_TABLE_NAME,FactionId ) of
		[]->[];
		[{_,Term}]-> Term
	end.

get_id(FactionRInfo)->
	?base_erlang:element(#faction_relations.id, FactionRInfo).

get_friendly(FactionRInfo)->
	?base_erlang:element(#faction_relations.friendly, FactionRInfo).

get_opposite(FactionRInfo)->
	?base_erlang:element(#faction_relations.opposite, FactionRInfo).
