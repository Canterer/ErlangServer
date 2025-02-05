%% Description: TODO: Add description to xxx server
-module(base_xxx_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("xxx_def.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
% -compile(export_all).
-export([
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {}).

%% --------------------------------------------------------------------
%%% External functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_init(_Args) ->
	{ok, #state{}}.

do_handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

do_handle_cast(_Msg, State) ->
	{noreply, State}.

do_handle_info(_Info, State) ->
	{noreply, State}.

do_terminate(_Reason, State) ->
	ok.

do_code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(OTP_FUNC_START, 'base_logger_util:otp_func_start').
-define(OTP_FUNC_END, 'base_logger_util:otp_func_end').

%% --------------------------------------------------------------------
%% Server functions
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(Args) ->
	?OTP_FUNC_START("~p:~p(Args=~p)~n",[?MODULE,?FUNCTION_NAME,Args]),
	Returns = do_init(Args),
	?OTP_FUNC_END("~p:~p Returns=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
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
	?OTP_FUNC_START("~p:~p(Request=~p, From=~p, State=~p)~n",[?MODULE,?FUNCTION_NAME,Request,From,State]),
	Returns = do_handle_call(Request, From, State),
	?OTP_FUNC_END("~p:~p Returns=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	?OTP_FUNC_START("~p:~p(Msg=~p, State=~p)~n",[?MODULE,?FUNCTION_NAME,Msg,State]),
	Returns = do_handle_cast(Msg, State),
	?OTP_FUNC_END("~p:~p Returns=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?OTP_FUNC_START("~p:~p(Info=~p, State=~p)~n",[?MODULE,?FUNCTION_NAME,Info,State]),
	Returns = do_handle_info(Info, State),
	?OTP_FUNC_END("~p:~p Returns=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	?OTP_FUNC_START("~p:~p(Reason=~p, State=~p)~n",[?MODULE,?FUNCTION_NAME,Reason,State]),
	Returns = do_terminate(Reason, State),
	?OTP_FUNC_END("~p:~p Returns=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	?OTP_FUNC_START("~p:~p(OldVsn=~p, State=~p, Extra=~p)~n",[?MODULE,?FUNCTION_NAME,OldVsn,State,Extra]),
	Returns = do_code_change(OldVsn, State, Extra),
	?OTP_FUNC_END("~p:~p Returns=~p~n",[?MODULE,?FUNCTION_NAME,Returns]),
	Returns.