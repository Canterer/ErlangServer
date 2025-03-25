%%% Description : base gen_statem
-module(base_gen_statem).
-include("base_define_shared.hrl").

-export([
	start/3,
	start/4,
	start_link/3,
	start_link/4,
	stop/1,
	stop/3,
	call/2,
	call/3,
	cast/2
]).

start(Module, Args, Opts) ->
	gen_statem:start(Module, Args, Opts).

start(ServerName, Module, Args, Opts) ->
	gen_statem:start(ServerName, Module, Args, Opts).

start_link(Module, Args, Opts) ->
	gen_statem:start_link(Module, Args, Opts).

start_link(ServerName, Module, Args, Opts) ->
	gen_statem:start_link(ServerName, Module, Args, Opts).

stop(ServerRef) ->
	gen_statem:stop(ServerRef).

stop(ServerRef, Reason, Timeout) ->
	gen_statem:stop(ServerRef, Reason, Timeout).

call(ServerRef, Request) ->
	gen_statem:call(ServerRef, Request).

call(ServerRef, Request, Timeout) ->
	gen_statem:call(ServerRef, Request, Timeout).

cast(ServerRef, Msg)->
	?OTP_FUNC_START("ServerRef=~p, Msg=~p",[ServerRef, Msg]),
	Returns = gen_statem:cast(ServerRef, Msg),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.