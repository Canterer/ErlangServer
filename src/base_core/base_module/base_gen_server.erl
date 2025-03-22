%%% Description : base gen_server

-module(base_gen_server).

-compile(export_all).

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
    gen_server:cast(Dest, Request).
