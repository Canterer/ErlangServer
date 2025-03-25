-module(base_erlang).

-include("base_define_shared.hrl").

-export([
	setelement/3,
	append_element/2,
	insert_element/3,
	delete_element/2
]).

setelement(Index, Tuple1, Value)->
	?TUPLE_OP_START("Index:~p, Tuple1:~p, Value:~p",[Index,Tuple1,Value]),
	Old = erlang:element(Index, Tuple1),
	Returns = erlang:setelement(Index, Tuple1, Value),
	New = erlang:element(Index, Tuple1),
	?TUPLE_OP_END("Old:~p New:~p Results=~p",[Old,New,Returns]),
	Returns.

append_element(Tuple1, Term)->
	?TUPLE_OP_START("Tuple1:~p, Term:~p",[Tuple1,Term]),
	Returns = erlang:append_element(Tuple1, Term),
	?TUPLE_OP_END("Results=~p",[Returns]),
	Returns.

insert_element(Index, Tuple1, Term)->
	?TUPLE_OP_START("Index:~p, Tuple1:~p, Term:~p",[Index,Tuple1,Term]),
	Returns = erlang:insert_element(Index, Tuple1, Term),
	?TUPLE_OP_END("Results=~p",[Returns]),
	Returns.

delete_element(Index, Tuple1)->
	?TUPLE_OP_START("Index:~p, Tuple1:~p",[Index,Tuple1]),
	Returns = erlang:delete_element(Index, Tuple1),
	?TUPLE_OP_END("Results=~p",[Returns]),
	Returns.
