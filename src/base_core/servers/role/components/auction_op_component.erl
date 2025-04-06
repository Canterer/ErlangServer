%% Description: TODO: Add description to auction_op_component
-module(auction_op_component).
-export([
	handle_event/4,
	load_from_db/2
]).

-include("base_component_shared.hrl").
-include("string_define.hrl").

load_from_db(StallName,Role_name)->
	%%摆摊
	case StallName of
		[]->
			auction_op:load_from_db(util:safe_binary_to_list(Role_name) ++ language:get_string(?STR_SELL_NAME));
		_->
			auction_op:load_from_db(StallName)
	end.

handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.