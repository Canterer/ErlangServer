%%% Description : base gen_statem

-module(base_gen_statem).

-compile(export_all).

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

cast({global,Name}, Msg) ->
    gen_statem:cast({global,Name}, Msg);
cast({via,RegMod,Name}, Msg) ->
    gen_statem:cast({via,RegMod,Name}, Msg);
cast({Name,Node} = ServerRef, Msg) when is_atom(Name), is_atom(Node)-> 
    gen_statem:cast({Name,Node}, Msg);
cast(ServerRef, Msg) when is_atom(ServerRef) ->
    gen_statem:cast(ServerRef, Msg);
cast(ServerRef, Msg) when is_pid(ServerRef) ->
    gen_statem:cast(ServerRef, Msg).