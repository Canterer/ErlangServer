-module(ets_operater_behaviour).

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
	base_logger_util:msg("~p:~p(Name:~p, Options:~p)~n",[?MODULE,?FUNCTION_NAME,Name,Options]),
	ets:new(Name, Options).

insert(Tab, ObjectOrObjects)->
	base_logger_util:msg("~p:~p(Tab:~p, ObjectOrObjects:~w)~n",[?MODULE,?FUNCTION_NAME,Tab,ObjectOrObjects]),
	ets:insert(Tab, ObjectOrObjects).

update_element(Tab, Key, ElementSpec)->
	base_logger_util:msg("~p:~p(Tab:~p, Key:~p, ElementSpec:~p)~n",[?MODULE,?FUNCTION_NAME,Tab,Key,ElementSpec]),
	ets:update_element(Tab, Key, ElementSpec).

delete(Tab)->
	base_logger_util:msg("~p:~p(Tab:~p)~n",[?MODULE,?FUNCTION_NAME,Tab]),
	ets:delete(Tab).

delete_object(Tab, Object)->
	base_logger_util:msg("~p:~p(Tab:~p, Object:~p)~n",[?MODULE,?FUNCTION_NAME,Tab,Object]),
	ets:delete_object(Tab, Object).

delete_all_objects(Tab)->
	base_logger_util:msg("~p:~p(Tab:~p)~n",[?MODULE,?FUNCTION_NAME,Tab]),
	ets:delete_all_objects(Tab).