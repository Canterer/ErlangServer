%% Description: base define shared
% -define(RELEASE,a).

-ifdef(RELEASE).%#################################################################
% ------------------去掉中间层-------------
-define(base_ets, ets).
-define(base_gen_server, gen_server).
-define(base_gen_statem, gen_statem).
-define(base_logger_util, error_logger).
% ------------------不交换不输出日志-------------
-define(init, init).
-define(log_init, log_init).
-define(handle_call, handle_call).
-define(log_handle_call, log_handle_call).
-define(handle_cast, handle_cast).
-define(log_handle_cast, log_handle_cast).
-define(handle_info, handle_info).
-define(log_handle_info, log_handle_info).
-define(terminate, terminate).
-define(log_terminate, log_terminate).
-define(code_change, code_change).
-define(log_code_change, log_code_change).


-define(handle_event, handle_event).
-define(log_handle_event, log_handle_event).


-define(create_ets, create_ets).
-define(log_create_ets, log_create_ets).
-define(init_ets, init_ets).
-define(log_init_ets, log_init_ets).


-define(start, start).
-define(log_start, log_start).
-define(create_mnesia_table, create_mnesia_table).
-define(log_create_mnesia_table, log_create_mnesia_table).
-define(create_mnesia_split_table, create_mnesia_split_table).
-define(log_create_mnesia_split_table, log_create_mnesia_split_table).
-define(delete_role_from_db, delete_role_from_db).
-define(log_delete_role_from_db, log_delete_role_from_db).
-define(tables_info, tables_info).
-define(log_tables_info, log_tables_info).
% ------------------不交换不输出日志-------------
-else.%#################################################################
% ------------------保留中间层-------------
-define(base_ets, base_ets).
-define(base_gen_server, base_gen_server).
-define(base_gen_statem, base_gen_statem).
-define(base_logger_util, base_logger_util).
% ------------------交换输出日志-------------
-define(init, log_init).
-define(log_init, init).
-define(handle_call, log_handle_call).
-define(log_handle_call, handle_call).
-define(handle_cast, log_handle_cast).
-define(log_handle_cast, handle_cast).
-define(handle_info, log_handle_info).
-define(log_handle_info, handle_info).
-define(terminate, log_terminate).
-define(log_terminate, terminate).
-define(code_change, log_code_change).
-define(log_code_change, code_change).


-define(handle_event, log_handle_event).
-define(log_handle_event, handle_event).


-define(create_ets, log_create_ets).
-define(log_create_ets, create_ets).
-define(init_ets, log_init_ets).
-define(log_init_ets, init_ets).


-define(start, log_start).
-define(log_start, start).
-define(create_mnesia_table, log_create_mnesia_table).
-define(log_create_mnesia_table, create_mnesia_table).
-define(create_mnesia_split_table, log_create_mnesia_split_table).
-define(log_create_mnesia_split_table, create_mnesia_split_table).
-define(delete_role_from_db, log_delete_role_from_db).
-define(log_delete_role_from_db, delete_role_from_db).
-define(tables_info, log_tables_info).
-define(log_tables_info, tables_info).
% ------------------交换输出日志-------------
-endif.%#################################################################



-define(SERVER, ?MODULE).
-define(ZSS(), ?base_logger_util:info_msg("#####ZZZZZ##### ~p:~p:line:~p~n",[?MODULE,?FUNCTION_NAME,?LINE])).
-define(ZSS(Format), ?base_logger_util:info_msg("#####ZZZZZ##### ~p:~p:line:~p("++Format++")~n",[?MODULE,?FUNCTION_NAME,?LINE])).
-define(ZSS(Format,Data), ?base_logger_util:info_msg("#####ZZZZZ##### ~p:~p:line:~p("++Format++")~n",[?MODULE,?FUNCTION_NAME,?LINE]++Data)).
-define(ZS_LOG(), ?base_logger_util:info_msg("#####ZZZZZ##### ~p:~p:line:~p~n",[?MODULE,?FUNCTION_NAME,?LINE])).
-define(ZS_LOG(Format), ?base_logger_util:info_msg("#####ZZZZZ##### ~p:~p:line:~p("++Format++")~n",[?MODULE,?FUNCTION_NAME,?LINE])).
-define(ZS_LOG(Format,Data), ?base_logger_util:info_msg("#####ZZZZZ##### ~p:~p:line:~p("++Format++")~n",[?MODULE,?FUNCTION_NAME,?LINE]++Data)).
% -define(ZS_LOG_E(Format,Data), ?base_logger_util:error_msg("#####ZZZZZ##### ~p:~p:line:~p("++Format++")~n",[?MODULE,?FUNCTION_NAME,?LINE]++Data)).
% -define(ZS_LOG(), ok).
% -define(ZS_LOG(Format, Data), ok).