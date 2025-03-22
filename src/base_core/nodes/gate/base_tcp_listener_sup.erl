%% Description: TODO: Add description to base_tcp_listener_sup
-module(base_tcp_listener_sup).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/4,
	start_link/5
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_sup_shared.hrl").

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Port, OnStartup, OnShutdown, AcceptCallback) ->
	start_link( Port, OnStartup, OnShutdown, AcceptCallback, 1).

start_link(Port, OnStartup, OnShutdown, AcceptCallback, AcceptorCount) ->
	supervisor:start_link({local,?MODULE},?MODULE, {Port, OnStartup, OnShutdown,
									AcceptCallback, AcceptorCount}).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init({ Port, OnStartup, OnShutdown, AcceptCallback, AcceptorCount}) ->
%% This is gross. The base_tcp_listener_server needs to know about the
%% tcp_acceptor_sup, and the only way I can think of accomplishing
%% that without jumping through hoops is to register the
%% tcp_acceptor_sup.
	{ok, {{one_for_all, 10, 10},[
			{base_tcp_acceptor_sup, 
				{base_tcp_acceptor_sup, start_link,[AcceptCallback]},
				transient, infinity, supervisor, [base_tcp_acceptor_sup]},
			{base_tcp_listener_server, 
				{base_tcp_listener_server, start_link,[Port,  AcceptorCount, OnStartup, OnShutdown]},
				transient, 100, worker, [base_tcp_listener_server]}
	]}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
