%%% Description : data modify processor of DAL
-module(base_db_dmp_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% double ets to save modify record 
-define(FLUSH_INTERVAL,1000*60*5).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last_ets}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link()->
	gen_server:start_link({local,?MODULE} ,?MODULE, [], []).
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}		  |
%%		  {ok, State, Timeout} |
%%		  ignore			   |
%%		  {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	base_logger_util:msg("~p:~p~n",[?MODULE,?FUNCTION_NAME]),
	base_timer_server:start_at_process(),
	FlushInterval = base_env_ets:get2(dmp, flush_interval, ?FLUSH_INTERVAL),
	base_db_dmp_util:init(),
	base_timer_util:send_after(FlushInterval, {flush_interval}),
	{ok, #state{last_ets=base_db_dmp_util:get_noinuse_ets()}}.

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
	Reply = ok,
	{reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%		  {noreply, State, Timeout} |
%%		  {stop, Reason, State}			(terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({flush_interval},  #state{last_ets=LastEts} = State) ->
	CurEts = base_db_dmp_util:get_noinuse_ets(),
	if (LastEts =:= CurEts)->
					 base_db_dmp_util:flush_not_using();
				 true->
		   			 nothing
	end,
	FlushInterval = base_env_ets:get2(dmp, flush_interval, ?FLUSH_INTERVAL),
	base_timer_util:send_after(FlushInterval, {flush_interval}),
	{noreply,#state{last_ets=CurEts}};
handle_info(Info, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
