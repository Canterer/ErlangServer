-module(base_ets).

-include("base_define_shared.hrl").

-export([
	new/2,
	insert/2,
	update_element/3,
	delete/1,
	delete_object/2,
	delete_all_objects/1,

	lookup/2,
	foldl/3,
	tab2list/1,
	match_object/1,
	match_object/2,
	match_object/3,
	info/1,
	info/2
]).

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

lookup(Tab, Key)->
	ets:lookup(Tab, Key).

foldl(Function, Acc0, Tab)->
	ets:foldl(Function, Acc0, Tab).

tab2list(Tab)->
	ets:tab2list(Tab).

match_object(Continuation)->
	ets:match_object(Continuation).
match_object(Tab, Pattern)->
	ets:match_object(Tab, Pattern).
match_object(Tab, Pattern, Limit)->
	ets:match_object(Tab, Pattern, Limit).

info(Tab)->
	ets:info(Tab).
info(Tab, Item)->
	ets:info(Tab, Item).
