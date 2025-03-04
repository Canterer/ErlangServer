%% Description: 本文件内含函数定义, include时需放在其他属性定义的最后
-include("base_define_shared.hrl").

-behaviour(gen_statem).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% --------------------------------------------------------------------
%% Server functions
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(Args) ->
	?OTP_FUNC_START("Args=~p",[Args]),
	Returns = do_init(Args),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: callback_mode/0
%% Description: return the callback mode
%% Returns: state_functions |
%%		  handle_event_function |
%%		  [state_functions, state_enter] |
%%		  [handle_event_function, state_enter] |
%% --------------------------------------------------------------------
callback_mode()	->
	?OTP_FUNC_START("",[]),
	Returns = do_callback_mode(),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_event/3
%% Description: State callback for all states when callback_mode() =:= state_functions.
%% EventType: enter | {'call',From :: from()} | 'cast' | 'info'
%% 					| 'timeout' | {'timeout', Name :: term()} | 'state_timeout' | 'internal'
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_event(EventType, EventContent, StateData) ->
	?OTP_FUNC_START("EventType=~p, EventContent=~p, StateData=~p",[EventType,EventContent,StateData]),
	Returns = do_handle_event(EventType, EventContent, StateData),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_event/4
%% Description: State callback for all states when callback_mode() =:= handle_event_function.
%% EventType: enter | {'call',From :: from()} | 'cast' | 'info'
%% 					| 'timeout' | {'timeout', Name :: term()} | 'state_timeout' | 'internal'
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_event(EventType, EventContent, StateName, StateData) ->
	?OTP_FUNC_START("EventType=~p, EventContent=~p, StateName=~p, StateData=~p",[EventType,EventContent,StateName,StateData]),
	Returns = do_handle_event(EventType, EventContent, StateName, StateData),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.


%% --------------------------------------------------------------------
%% Function: terminate/3
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
	?OTP_FUNC_START("Reason=~p, StateName=~p, StateData=~p",[Reason,StateName,StateData]),
	Returns = do_terminate(Reason, StateName, StateData),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
	?OTP_FUNC_START("OldVsn=~p, StateName=~p, StateData=~p, Extra=~p",[OldVsn,StateName,StateData,Extra]),
	Returns = do_code_change(OldVsn, StateName, StateData, Extra),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.
%% ---------------------