%% Description: 本文件内含函数定义, include时需放在其他属性定义的最后
-include("base_define_shared.hrl").

% 自定义本模块宏定义 开启间接接口
% -undef(init).
% -undef(log_init).
% -define(init, log_init).
% -define(log_init, init).
% -undef(handle_event).
% -undef(log_handle_event).
% -define(handle_event, log_handle_event).
% -define(log_handle_event, handle_event).
% -undef(terminate).
% -undef(log_terminate).
% -define(terminate, log_terminate).
% -define(log_terminate, terminate).
% -undef(code_change).
% -undef(log_code_change).
% -define(code_change, log_code_change).
% -define(log_code_change, code_change).


-ifndef(CALLBACK_MODE).
-define(CALLBACK_MODE, 	handle_event_function).
-endif.
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).
-behaviour(gen_statem).

-include("base_component_shared.hrl").

%% --------------------------------------------------------------------
%% Server functions
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
?log_init(Args) ->
	?OTP_FUNC_START("Args=~p",[Args]),
	Returns = ?init(Args),
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
	?CALLBACK_MODE.

%% --------------------------------------------------------------------
%% Function: user_state_name/3
%% Description: State callback for all states when callback_mode() =:= state_functions.
%% EventType: enter | {'call',From :: from()} | 'cast' | 'info'
%% 					| 'timeout' | {'timeout', Name :: term()} | 'state_timeout' | 'internal'
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
do_handle_state_event(EventType, EventContent, StateName, StateData) ->
	?log_handle_event(EventType, EventContent, StateName, StateData).

%% --------------------------------------------------------------------
%% Function: handle_event/4
%% Description: State callback for all states when callback_mode() =:= handle_event_function.
%% EventType: enter | {'call',From :: from()} | 'cast' | 'info'
%% 					| 'timeout' | {'timeout', Name :: term()} | 'state_timeout' | 'internal'
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
?log_handle_event(EventType, EventContent, StateName, StateData) ->
	?OTP_FUNC_START("EventType=~p, EventContent=~p, StateName=~p, StateData=~p",[EventType,EventContent,StateName,StateData]),
	Returns = ?handle_event(EventType, EventContent, StateName, StateData),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.


%% --------------------------------------------------------------------
%% Function: terminate/3
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
?log_terminate(Reason, StateName, StateData) ->
	?OTP_FUNC_START("Reason=~p, StateName=~p, StateData=~p",[Reason,StateName,StateData]),
	Returns = ?terminate(Reason, StateName, StateData),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
?log_code_change(OldVsn, StateName, StateData, Extra) ->
	?OTP_FUNC_START("OldVsn=~p, StateName=~p, StateData=~p, Extra=~p",[OldVsn,StateName,StateData,Extra]),
	Returns = ?code_change(OldVsn, StateName, StateData, Extra),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.
%% ---------------------