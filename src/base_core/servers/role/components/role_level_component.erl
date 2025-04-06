%% Description: TODO: Add description to role_level_component
-module(role_level_component).
-export([
	handle_event/4,
	init_current_exp/2,
	get_next_level_exp/1
]).

-include("base_component_shared.hrl").

init_current_exp(Expr,Level)->
	NowLevelExp = role_level_db:get_level_experience(Level),	
	put(current_exp,Expr+NowLevelExp).

get_next_level_exp(RoleLevel)->
	NowLevelExp = role_level_db:get_level_experience(RoleLevel),
	NexLevelexp = role_level_db:get_level_experience(RoleLevel+1),
	case NexLevelexp of
		noleve -> 0;
		_ -> %%LevelupExp = erlang:min(2147483647,NexLevelexp - NowLevelExp)	%%升级所需经验
			NexLevelexp - NowLevelExp
	end.
handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.