%%% Description : base_component_shared
-ifndef(BASE_DEFINE_MIN).
-include("base_define_min.hrl").
-endif.

-ifndef(LOGIN_PB_RECORD).
-include("login_pb.hrl").
-endif.

-export([
	apply_component/3,
	apply_component_handle/4
]).

apply_component(Component,Func,Args)->
	base_component_util:applyComponentFunc(?MODULE,Component,Func,Args).

apply_component_handle(Event, EventContent, StateName, StateData)->
	Components = base_component_util:getComponents(),
	Result = lists:foldl(fun(Component,HandledResult)->
			case HandledResult of
				unhandle ->
					base_component_util:applyComponentFunc(?MODULE,Component,handle_event,[Event, EventContent, StateName, StateData]);
				_ ->
					HandledResult
			end
	end,unhandle,Components),
	if
		Result =/= unhandle ->
			Result;
		true ->
			case Event of
				{call, From} ->
					base_logger_util:info_msg("~p handle_event undefined event:({call, From:~p}, EventContent:~p, StateName:~p, StateData:~p) !!!!!~n",[?MODULE, From, EventContent, StateName, StateData]),
					{keep_state_and_data, [{reply, From, ok}]};
				_ ->
					base_logger_util:info_msg("~p handle_event undefined event:(Event:~p, EventContent:~p, StateName:~p, StateData:~p) !!!!!~n",[?MODULE, Event, EventContent, StateName, StateData]),
					keep_state_and_data
			end
	end.