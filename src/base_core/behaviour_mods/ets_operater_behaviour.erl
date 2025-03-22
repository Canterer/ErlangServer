-module(ets_operater_behaviour).

-export([behaviour_info/1]).
% -export([
% ]).

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
