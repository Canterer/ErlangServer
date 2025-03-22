%% Description: 本文件内含函数定义, include时需放在其他属性定义的最后
-include("base_define_shared.hrl").

% 自定义本模块宏定义 开启间接接口
% -undef(init).
% -undef(log_init).
% -define(init, log_init).
% -define(log_init, init).
% -undef(handle_call).
% -undef(log_handle_call).
% -define(handle_call, log_handle_call).
% -define(log_handle_call, handle_call).
% -undef(handle_cast).
% -undef(log_handle_cast).
% -define(handle_cast, log_handle_cast).
% -define(log_handle_cast, handle_cast).
% -undef(handle_info).
% -undef(log_handle_info).
% -define(handle_info, log_handle_info).
% -define(log_handle_info, handle_info).
% -undef(terminate).
% -undef(log_terminate).
% -define(terminate, log_terminate).
% -define(log_terminate, terminate).
% -undef(code_change).
% -undef(log_code_change).
% -define(code_change, log_code_change).
% -define(log_code_change, code_change).


%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

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
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}		  |
%%		  {reply, Reply, State, Timeout} |
%%		  {noreply, State}			   |
%%		  {noreply, State, Timeout}	  |
%%		  {stop, Reason, Reply, State}   | (terminate/2 is called)
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
?log_handle_call(Request, From, State) ->
	?OTP_FUNC_START("Request=~p, From=~p, State=~p",[Request,From,State]),
	Returns = ?handle_call(Request, From, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
?log_handle_cast(Msg, State) ->
	?OTP_FUNC_START("Msg=~p, State=~p",[Msg,State]),
	Returns = ?handle_cast(Msg, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
?log_handle_info(Info, State) ->
	?OTP_FUNC_START("Info=~p, State=~p",[Info,State]),
	Returns = ?handle_info(Info, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
?log_terminate(Reason, State) ->
	?OTP_FUNC_START("Reason=~p, State=~p",[Reason,State]),
	Returns = ?terminate(Reason, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
?log_code_change(OldVsn, State, Extra) ->
	?OTP_FUNC_START("OldVsn=~p, State=~p, Extra=~p",[OldVsn,State,Extra]),
	Returns = ?code_change(OldVsn, State, Extra),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.
%% ---------------------