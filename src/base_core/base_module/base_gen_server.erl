%%% Description : base gen_server
-module(base_gen_server).
-include("base_define_shared.hrl").

-export([
	start/3,
	start/4,
	start_link/3,
	start_link/4,
	call/2,
	call/3,
	cast/2
]).

start(Mod, Args, Options) ->
	gen_server:start(Mod, Args, Options).

start(Name, Mod, Args, Options) ->
	gen_server:start(Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
	gen_server:start_link(Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
	gen_server:start_link(Name, Mod, Args, Options).

call(Name, Request) ->
	gen_server:call(Name, Request).

call(Name, Request, Timeout) ->
	gen_server:call(Name, Request, Timeout).

cast(Dest, Request) ->
	?OTP_FUNC_START("Dest=~p, Request=~p",[Dest, Request]),
	Returns = gen_server:cast(Dest, Request),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.
