%% Description: TODO: Add description to base_tcp_acceptor_server
-module(base_tcp_acceptor_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/3,
	get_proc_name/1,
	disable_connect/1,
	enable_connect/1
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {callback, sock, ref,disable_connect}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_server_shared.hrl").

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------
%%% put(key, value)、get(key)在进程字典中存储和检索键值对
%%% 进程字典是一个进程独有的、存储键值对的数据结构
%% --------------------------------------------------------------------
start_link(Callback, LSock,AcceptorIndex) ->
    base_gen_server:start_link(?MODULE, {Callback, LSock,AcceptorIndex}, []).


disable_connect(NamedProc)->
	case erlang:whereis(NamedProc) of
		undefined-> ignor;
		Pid-> base_gen_server:call(Pid, {disable_connect})
	end.

enable_connect(NamedProc)->
	case erlang:whereis(NamedProc) of
		undefined-> ignor;
		Pid-> base_gen_server:call(Pid, {enable_connect})
	end.

get_proc_name(AcceptorIndex)->
	list_to_atom("acceptor_"++integer_to_list(AcceptorIndex)).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_init({Callback, LSock,AcceptorIndex}) ->
	base_logger_util:msg("~p:~p({Callback:~p, LSock:~p, AcceptorIndex:~p})~n",[?MODULE,?FUNCTION_NAME,Callback,LSock,AcceptorIndex]),
	%%make acceptor name
	erlang:register(get_proc_name(AcceptorIndex), self()),
    base_gen_server:cast(self(), accept),
    {ok, #state{callback=Callback, sock=LSock,disable_connect=false}}.

do_handle_call({disable_connect}, _From, State) ->
    Reply = State,
    {reply, Reply, State#state{disable_connect=true}};
do_handle_call({enable_connect}, _From, State) ->
    Reply = State,
    {reply, Reply, State#state{disable_connect=false}};
do_handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

do_handle_cast(accept, State) ->
    accept(State);
do_handle_cast(_Msg, State) ->
	{noreply, State}.

do_handle_info({inet_async, LSock, Ref, {ok, Sock}},
            State = #state{callback={M,F,A}, sock=LSock, ref=Ref,disable_connect=Disable}) ->
	{ok, Mod} = inet_db:lookup_socket(LSock),
	inet_db:register_socket(Sock, Mod),
	try
		{Address, Port}         = inet_op(fun () -> inet:sockname(LSock) end),
		{PeerAddress, PeerPort} = inet_op(fun () -> inet:peername(Sock) end),
		base_logger_util:msg("accepted TCP connection on ~s:~p from ~s:~p~n",[inet_parse:ntoa(Address), Port,inet_parse:ntoa(PeerAddress), PeerPort]),
		{ok, ChildPid} = supervisor:start_child(base_tcp_client_sup, []),
		ok = gen_tcp:controlling_process(Sock, ChildPid),
		case Disable of
			true->
				base_tcp_client_fsm:socket_disable(node(),ChildPid,Sock);
			false->
				base_tcp_client_fsm:socket_ready(node(),ChildPid,Sock)
		end,
		apply(M, F, A ++ [Sock,ChildPid])
	catch
		{inet_error, Reason} ->
				gen_tcp:close(Sock),
				base_logger_util:error_msg("unable to accept TCP connection: ~p~n",[Reason]);
		EXP->
				base_logger_util:error_msg("unable to accept TCP connection: ~p~n",[EXP])
	end,
	accept(State);
do_handle_info({inet_async, LSock, Ref, {error, closed}},
            State=#state{sock=LSock, ref=Ref}) ->
    %% It would be wrong to attempt to restart the acceptor when we
    %% know this will fail.
    {stop, normal, State};
do_handle_info(_Info, State) ->
	{noreply, State}.

do_terminate(_Reason, _State) ->
	ok.

do_code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
throw_on_error(E, Thunk) ->
    case Thunk() of
        {error, Reason} -> throw({E, Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

inet_op(F) -> throw_on_error(inet_error, F).

accept(State = #state{sock=LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     -> {stop, {cannot_accept, Error}, State}
    end.
