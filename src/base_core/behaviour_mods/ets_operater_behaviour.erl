-module(ets_operater_behaviour).

% -export([behaviour_info/1]).

% -callback create_ets() ->
% 	term().
% -callback init_ets() ->
% 	term().

% -optional_callbacks([create_ets/0, init_ets/0]).

behaviour_info(callbacks) ->
[
	{create_ets,0},							%% create ets
	{init_ets,0}			   				%% init mod	-> load to ets from db. define in  option/game_server.option
];
behaviour_info(_Other) ->
	undefined.
