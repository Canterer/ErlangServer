%% Description: base define shared
-define(SERVER, ?MODULE).

-define(ZS_LOG(), base_logger_util:msg("#####~p:~p:line:~p~n",[?MODULE,?FUNCTION_NAME,?LINE])).
-define(ZS_LOG(Format,Data), base_logger_util:msg("#####~p:~p:line:~p("++Format++")~n",[?MODULE,?FUNCTION_NAME,?LINE]++Data)).
% -define(ZS_LOG(), ok).
% -define(ZS_LOG(Format, Data), ok).

% -define(FUNC_LINE_LOG, base_logger_util:msg("#####~p:line:~p~n",[?MODULE,?LINE])).
% -define(FUNC_LINE_LOG, ok).

-define(OTP_FUNC_START(Format, Data), base_logger_util:msg("+++~p:~p("++Format++")~n", [?MODULE,?FUNCTION_NAME]++Data)).
-define(OTP_FUNC_END(Format, Data), base_logger_util:msg("+++~p:~p("++Format++")~n", [?MODULE,?FUNCTION_NAME]++Data)).
% -define(OTP_FUNC_START(Format, Data), ok).
% -define(OTP_FUNC_END(Format, Data), ok).

% -define(DB_OPERATER_STARTA,  'base_logger_util').
% -define(DB_OPERATER_STARTB,  'db_operater_start').
% -define(DB_OPERATER_START,  ?DB_OPERATER_STARTA:?DB_OPERATER_STARTB).
-define(DB_OPERATER_START(),  base_logger_util:msg("DBDBD~p:~p() begin!!!~n", [?MODULE,?FUNCTION_NAME])).
-define(DB_OPERATER_START(Format, Data),  base_logger_util:msg("DBDBD~p:~p("++Format++") begin!!!~n", [?MODULE,?FUNCTION_NAME,?LINE]++Data)).
-define(DB_OPERATER_END(), base_logger_util:msg("DBDBD~p:~p end!!!~n",[?MODULE,?FUNCTION_NAME])).
% -define(DB_OPERATER_START(), ok).
% -define(DB_OPERATER_START(Format, Data), ok).
% -define(DB_OPERATER_END, ok).

% -define(ETS_OPERATER_START(Format, Data), base_logger_util:msg("+++"++Format, Data)).
% -define(ETS_OPERATER_END(Format, Data), base_logger_util:msg("+++"++Format, Data)).
-define(ETS_OPERATER_START(Format, Data), ok).
-define(ETS_OPERATER_END(Format, Data), ok).

