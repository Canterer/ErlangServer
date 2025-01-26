%%% Description : fsm 有限状态机

-module(base_fsm_util).

-compile(export_all).

-define(SEND_EVENT, gen_fsm:send_event).
% -define(SEND_EVENT, gen_statem:cast).
-define(SYNC_SEND_EVENT, gen_fsm:sync_send_event).
% -define(SYNC_SEND_EVENT, gen_statem:cast).

start_link(Mod, Args, Options) ->
    gen_fsm:start_link(Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen_fsm:start_link(Name, Mod, Args, Options).

send_state_event(Pid,Event)->
	?SEND_EVENT(Pid,Event).

send_state_event(Node,ProcName,Event)->
	CurNode = node(),
	case Node of
		CurNode -> 
			?SEND_EVENT(ProcName,Event);
		_ ->
			?SEND_EVENT({ProcName,Node}, Event)
	end.

sync_send_state_event(Node, ProcName, Event)->
	CurNode = node(),
	case Node of
		CurNode -> 
			gen_fsm:sync_send_event(ProcName,Event);
		_ ->
			gen_fsm:sync_send_event({ProcName,Node}, Event)
	end.

sync_send_state_event(Node, ProcName, Event, Timeout)->
	CurNode = node(),
	case Node of
		CurNode -> 
			gen_fsm:sync_send_event(ProcName, Event, Timeout);
		_ ->
			gen_fsm:sync_send_event({ProcName, Node}, Event, Timeout)
	end.

send_all_state_event(Name, Event)->
	gen_fsm:send_all_state_event(Name, Event).

sync_send_all_state_event(Name, Event)->
	gen_fsm:sync_send_all_state_event(Name, Event).

sync_send_all_state_event(Name, Event, Timeout)->
	gen_fsm:sync_send_all_state_event(Name, Event, Timeout).