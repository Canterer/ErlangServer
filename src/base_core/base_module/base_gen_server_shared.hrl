%% Description: 本文件内含函数定义, include时需放在其他属性定义的最后
-include("base_define_shared.hrl").

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}		  |
%%		  {reply, Reply, State, Timeout} |
%%		  {noreply, State}			   |
%%		  {noreply, State, Timeout}	  |
%%		  {stop, Reason, Reply, State}   | (terminate/2 is called)
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
	?OTP_FUNC_START("Request=~p, From=~p, State=~p",[Request,From,State]),
	Returns = do_handle_call(Request, From, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	?OTP_FUNC_START("Msg=~p, State=~p",[Msg,State]),
	Returns = do_handle_cast(Msg, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?OTP_FUNC_START("Info=~p, State=~p",[Info,State]),
	Returns = do_handle_info(Info, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?OTP_FUNC_START("Reason=~p, State=~p",[Reason,State]),
	Returns = do_terminate(Reason, State),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	?OTP_FUNC_START("OldVsn=~p, State=~p, Extra=~p",[OldVsn,State,Extra]),
	Returns = do_code_change(OldVsn, State, Extra),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.
%% ---------------------