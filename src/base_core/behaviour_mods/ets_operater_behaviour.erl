-module(ets_operater_behaviour).

-include("base_define_shared.hrl").

-export([behaviour_info/1]).
-export([
	new/2,
	insert/2,
	update_element/3,
	delete/1,
	delete_object/2,
	delete_all_objects/1
]).

%%
%%	behaviour fun	-behaviour(ets_operater_behaviour).
%%	copy this:		-export([init/0,create/0]).
%%

behaviour_info(callbacks) ->
[
	{create_ets,0},							%% create ets
	{init_ets,0}			   				%% init mod	-> load to ets from db. define in  option/game_server.option
];
behaviour_info(_Other) ->
	undefined.


new(Name, Options)->
	?ETS_OPERATER_START("~p:~p(Name:~p, Options:~p)~n",[?MODULE,?FUNCTION_NAME,Name,Options]),
	Returns = ets:new(Name, Options),
	?ETS_OPERATER_END("~p:~p Results=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

insert(Tab, ObjectOrObjects)->
	?ETS_OPERATER_START("~p:~p(Tab:~p, ObjectOrObjects:~w)~n",[?MODULE,?FUNCTION_NAME,Tab,ObjectOrObjects]),
	Returns = ets:insert(Tab, ObjectOrObjects),
	?ETS_OPERATER_END("~p:~p Results=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

update_element(Tab, Key, ElementSpec)->
	?ETS_OPERATER_START("~p:~p(Tab:~p, Key:~p, ElementSpec:~p)~n",[?MODULE,?FUNCTION_NAME,Tab,Key,ElementSpec]),
	Returns = ets:update_element(Tab, Key, ElementSpec),
	?ETS_OPERATER_END("~p:~p Results=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

delete(Tab)->
	?ETS_OPERATER_START("~p:~p(Tab:~p)~n",[?MODULE,?FUNCTION_NAME,Tab]),
	Returns = ets:delete(Tab),
	?ETS_OPERATER_END("~p:~p Results=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

delete_object(Tab, Object)->
	?ETS_OPERATER_START("~p:~p(Tab:~p, Object:~p)~n",[?MODULE,?FUNCTION_NAME,Tab,Object]),
	Returns = ets:delete_object(Tab, Object),
	?ETS_OPERATER_END("~p:~p Results=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

delete_all_objects(Tab)->
	?ETS_OPERATER_START("~p:~p(Tab:~p)~n",[?MODULE,?FUNCTION_NAME,Tab]),
	Returns = ets:delete_all_objects(Tab),
	?ETS_OPERATER_END("~p:~p Results=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.