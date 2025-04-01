%%% Description : base_component_util
-module(base_component_util).

-export([
	getComponents/0,
	applyComponentFunc/4
]).

-define(ComponentName(Component), list_to_atom(atom_to_list(components_) ++ atom_to_list(Component))).

getComponents()->
	case get(components) of
		undefined->
			TempRes = lists:foldl(fun({Component,Active}, Result)->
				if
					Active ->
						[Component|Result];
					true ->
						Result
				end
			end,[],base_env_ets:get(components,[])),
			Res = lists:reverse(TempRes),
			put(components,Res),
			Res;
		Res->
			Res
	end.

checkComponent(Component)->
	case get(?ComponentName(Component)) of
		undefined->
			Active = base_env_ets:get2(components,Component,false),
			put(?ComponentName(Component),Active),
			Active;
		Res->
			Res
	end.

applyComponentFunc(Mod,Component,Func,Args)->
	if
		Func =/= handle_event ->
			base_logger_util:info_msg("#####CCCCC##### Mod:~p applyComponentFunc(Component:~p FuncName:~p Args:~p)",[Mod,Component,Func,Args]);
		true->
			ok
	end,
	% base_logger_util:info_msg("Mod:~p applyComponentFunc(Component:~p FuncName:~p Args:~p)",[Mod,Component,Func,Args]),
	case checkComponent(Component) of
		true ->
			Res = erlang:apply(Component,Func,Args),
			if
				Func == handle_event and Res =/= unhandle) ->
					base_logger_util:info_msg("#####CCCCC##### Mod:~p applyComponentFunc(Component:~p FuncName:~p Args:~p)",[Mod,Component,Func,Args]);
				true->
					ok
			end,
			Res;
		false ->
			base_logger_util:info_msg("#####CCCCC##### checkComponent ~p failed !!!!!!!!!!!!!!!!!!!!!",[Component]),
			failed
	end.






% defaultReturn(Func)->
% 	Func().

% continue(Func)->
% 	Func().

% branch(Func)->
% 	Func().