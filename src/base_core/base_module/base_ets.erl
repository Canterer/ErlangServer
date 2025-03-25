-module(base_ets).

-include("base_define_shared.hrl").

-export([
	new/2,
	insert/2,
	update_element/3,
	delete/1,
	delete/2,
	delete_object/2,
	delete_all_objects/1,

	lookup/2,
	foldl/3,
	tab2list/1,
	match/1,
	match/2,
	match/3,
	match_object/1,
	match_object/2,
	match_object/3,
	info/1,
	info/2
]).

new(Name, Options)->
	?ETS_OPERATER_START("Name:~p, Options:~p",[Name,Options]),
	Returns = ets:new(Name, Options),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.

insert(Tab, ObjectOrObjects)->
	?ETS_OPERATER_START("Tab:~p, ObjectOrObjects:~w",[Tab,ObjectOrObjects]),
	Returns = ets:insert(Tab, ObjectOrObjects),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.

update_element(Tab, Key, ElementSpec)->
	?ETS_OPERATER_START("Tab:~p, Key:~p, ElementSpec:~p",[Tab,Key,ElementSpec]),
	Returns = ets:update_element(Tab, Key, ElementSpec),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.

delete(Tab)->
	?ETS_OPERATER_START("Tab:~p",[Tab]),
	Returns = ets:delete(Tab),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.

delete(Tab, Key)->
	?ETS_OPERATER_START("Tab:~p, Key:~p",[Tab,Key]),
	Returns = ets:delete(Tab, Key),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.

delete_object(Tab, Object)->
	?ETS_OPERATER_START("Tab:~p, Object:~p",[Tab,Object]),
	Returns = ets:delete_object(Tab, Object),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.

delete_all_objects(Tab)->
	?ETS_OPERATER_START("Tab:~p",[Tab]),
	Returns = ets:delete_all_objects(Tab),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.

lookup(Tab, Key)->
	% ?ETS_OPERATER_START("Tab:~p, Key:~p",[Tab,Key]),
	% Returns = ets:lookup(Tab, Key),
	% ?ETS_OPERATER_END("Results=~p",[Returns]),
	% Returns.
	ets:lookup(Tab, Key).

foldl(Function, Acc0, Tab)->
	ets:foldl(Function, Acc0, Tab).

tab2list(Tab)->
	ets:tab2list(Tab).

match(Continuation)->
	ets:match(Continuation).
match(Tab, Pattern)->
	ets:match(Tab, Pattern).
match(Tab, Pattern, Limit)->
	ets:match(Tab, Pattern, Limit).

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
