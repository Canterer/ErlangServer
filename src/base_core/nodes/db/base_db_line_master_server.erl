%% Description: TODO: Add description to base_db_line_master_server
-module(base_db_line_master_server).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------

-export([
	start_link/0,
	is_db_prepread/1
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {}).

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
start_link()->
	?base_gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

is_db_prepread(Node)->
	base_logger_util:info_msg("~p:~p(Node:~p)~n",[?MODULE,?FUNCTION_NAME,Node]),
	try
		?base_gen_server:call({?SERVER,Node}, is_db_prepread)
	catch
		E:R->
			base_logger_util:info_msg("base_db_line_master_server error no_proc ,wait ~n "),
			false
	end.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init(_Args) ->
	?ZS_LOG(),
	put(db_prepare_finish,false),
	?ZS_LOG(),
	base_db_init_util:db_init_line_master(),
	?ZS_LOG(),
	put(db_prepare_finish,true),
	base_logger_util:info_msg("line's ram db prepare finished!! ~n"),
	{ok, #state{}}.

?handle_call(is_db_prepread, From, State) ->
	Reply = get(db_prepare_finish),
	{reply, Reply, State};
?handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

?handle_cast(_Msg, State) ->
	{noreply, State}.

?handle_info(_Info, State) ->
	{noreply, State}.

?terminate(_Reason, _State) ->
	ok.

?code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Not export functions
%% --------------------------------------------------------------------
