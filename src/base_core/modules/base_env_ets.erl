-module(base_env_ets).
-include("base_define_min.hrl").

-define(OPTION_ETS,option_value_ets).
-define(SERVER_NAME_ETS,option_servers_name).
-define(BASE_NODES_OPTION_FILE,"../src/base_core/option/base_nodes_config.option").
-define(BASE_DB_OPTION_FILE,"../src/base_core/option/base_db_config.option").
-define(BASE_LINE_OPTION_FILE,"../src/base_core/option/base_line_config.option").
-define(BASE_MAP_OPTION_FILE,"../src/base_core/option/base_map_config.option").
-define(BASE_GATE_OPTION_FILE,"../src/base_core/option/base_gate_config.option").
-define(BASE_CROSSDOMAIN_OPTION_FILE,"../src/base_core/option/base_crossdomain_config.option").
-define(BASE_GM_OPTION_FILE,"../src/base_core/option/base_gm_config.option").
-define(BASE_AUTH_OPTION_FILE,"../src/base_core/option/base_auth_config.option").
-define(BASE_CHAT_OPTION_FILE,"../src/base_core/option/base_chat_config.option").
-define(BASE_GUILD_OPTION_FILE,"../src/base_core/option/base_guild_config.option").

%%
%% Exported Functions
%%

-export([get_env/2,get/2,get2/3,put/2,put2/3,get_tuple2/3]).
-export([init/0,reset/0,get_server_name/1]).
%%
%% API Functions
%%

%%
%% Local Functions
%%

get_env(Opt, Default) ->
	case application:get_env(Opt) of
		{ok, Val} ->  Val;
		_ -> Default
	end.

init()->
	try
		?base_ets:new(?OPTION_ETS, [named_table,public,set])
	catch
		_:_-> ignor
	end,
	try
		?base_ets:new(?SERVER_NAME_ETS, [named_table,public,set])
	catch
		_:_-> ignor
	end.

read_from_file(File,Ets)->
	base_logger_util:info_msg("base_env_ets init ets ~p from option file:~p~n",[Ets,File]),
	% case file:read_file(File) of
	% 	{ok, Binary}->
	% 		case io:read(Binary) of
	% 			{ok, Term} ->
	% 				lists:foreach(fun(Term)->
	% 						?base_ets:insert(Ets, Term)
	% 					end, Terms);
	% 			{error, ErrorInfo} ->
	% 				base_logger_util:info_msg("load option file [~p] Error ~p~n",[File,Reason]),	
	% 				{error, ErrorInfo}
	% 		end;
	% 	{error, Reason} -> 
	% 		base_logger_util:info_msg("load option file [~p] Error ~p~n",[File,Reason]),	
	% 		{error, Reason}
	% end.
	case file:consult(File) of
		{ok, [Terms]} ->
			lists:foreach(fun(Term)->
								  ?base_ets:insert(Ets, Term)
						  end, Terms);
		{error, Reason} -> 
			base_logger_util:info_msg("load option file [~p] Error ~p~n",[File,Reason]),	
			{error, Reason}
	end.

get(Key,Default)->
	try
		case ?base_ets:lookup(?OPTION_ETS, Key) of
			[]-> Default;
			[{_,Value}]->Value
		end
	catch
		E:R->
			base_logger_util:info_msg("env get error !!!!!!!!! Key ~p R ~p  ~p ~n",[Key,R,erlang:get_stacktrace()]),
			Default
	end.

get2(Key,Key2,Default)->
	case get(Key,[]) of
		[]->Default;
		Value-> case lists:keyfind(Key2, 1, Value) of
					false-> Default;
					{_Key2,Value2}-> Value2
				end
	end.

get_tuple2(Key,Key2,Default)->
	case get(Key,[]) of
		[]->Default;
		Value-> case lists:keyfind(Key2, 1, Value) of
					false-> Default;
					Tuple-> Tuple
				end
	end.

put(Key,Value)->
	?base_ets:insert(?OPTION_ETS, {Key,Value}).

put2(Key,Key2,Value)->
	OldValue = get(Key,[{Key2,Value}]),
	NewValue = lists:keyreplace(Key2, 1, OldValue, {Key2,Value}),
	?base_ets:insert(?OPTION_ETS, {Key,NewValue}).

reset()->
	?base_ets:delete_all_objects(?OPTION_ETS),
	?base_ets:delete_all_objects(?SERVER_NAME_ETS),
	read_from_file(?BASE_NODES_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_DB_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_LINE_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_MAP_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_GATE_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_CROSSDOMAIN_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_GM_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_AUTH_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_CHAT_OPTION_FILE,?OPTION_ETS),
	read_from_file(?BASE_GUILD_OPTION_FILE,?OPTION_ETS),
	ok.


get_server_name(ServerId)->
	try
		case ?base_ets:lookup(?SERVER_NAME_ETS, ServerId) of
			[]-> [];
			[{ServerId,ServerName}]-> ServerName
		end
	catch
		E:R->
			base_logger_util:info_msg("get_server_name error !!!!!!!!! ServerId ~p R ~p  ~p ~n",[ServerId,R,erlang:get_stacktrace()]),
			[]
	end.