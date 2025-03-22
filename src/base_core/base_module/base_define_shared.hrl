%% Description: base define shared
-include("base_define_min.hrl").

-define(OTP_FUNC_START(Format, Data), ?base_logger_util:info_msg("#####OOOOO##### ~p:~p("++Format++") begin!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
-define(OTP_FUNC_END(Format, Data), ?base_logger_util:info_msg("#####OOOOO##### ~p:~p("++Format++") end!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
% -define(OTP_FUNC_START(Format, Data), ok).
% -define(OTP_FUNC_END(Format, Data), ok).


-define(DB_OPERATER_START(),  ?base_logger_util:info_msg("#####DDDDD##### ~p:~p() begin!!!~n", [?MODULE,?FUNCTION_NAME])).
-define(DB_OPERATER_START(Format, Data),  ?base_logger_util:info_msg("#####DDDDD##### ~p:~p("++Format++") begin!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
-define(DB_OPERATER_END(), ?base_logger_util:info_msg("#####DDDDD##### ~p:~p end!!!~n",[?MODULE,?FUNCTION_NAME])).
% -define(DB_OPERATER_START(), ok).
% -define(DB_OPERATER_START(Format, Data), ok).
% -define(DB_OPERATER_END, ok).


-define(ETS_OPERATER_START(Format, Data), ?base_logger_util:info_msg("#####EEEEE##### ~p:~p("++Format++") begin!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
-define(ETS_OPERATER_END(Format, Data), ?base_logger_util:info_msg("#####EEEEE##### ~p:~p("++Format++") end!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
% -define(ETS_OPERATER_START(Format, Data), ok).
% -define(ETS_OPERATER_END(Format, Data), ok).


-define(BEHAVIOUR_FUNC_START(), ?base_logger_util:info_msg("#####BBBBB##### ~p:~p() begin!!!~n", [?MODULE,?FUNCTION_NAME])).
-define(BEHAVIOUR_FUNC_START(Format, Data), ?base_logger_util:info_msg("#####BBBBB##### ~p:~p("++Format++") begin!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
-define(BEHAVIOUR_FUNC_END(Format, Data), ?base_logger_util:info_msg("#####BBBBB##### ~p:~p("++Format++") end!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
% -define(BEHAVIOUR_FUNC_START(), ok).
% -define(BEHAVIOUR_FUNC_START(Format, Data), ok).
% -define(BEHAVIOUR_FUNC_END(Format, Data), ok).

-define(MNESIA_OPERATER(Format, Data), ?base_logger_util:info_msg("#####MMMMM##### ~p:~p("++Format++") begin!!!~n", [?MODULE,?FUNCTION_NAME]++Data)).
-define(MNESIA_OPERATER_RESULT(Format, Data), ?base_logger_util:info_msg("#####MMMMM##### ~p:~p"++Format++"~n", [?MODULE,?FUNCTION_NAME]++Data)).
% -define(MNESIA_OPERATER(Format, Data), ok).
% -define(MNESIA_OPERATER_RESULT(Format, Data), ok).

% -define(DB_OPERATER_STARTA,  '?base_logger_util').
% -define(DB_OPERATER_STARTB,  'db_operater_start').
% -define(DB_OPERATER_START,  ?DB_OPERATER_STARTA:?DB_OPERATER_STARTB).
% -define(FUNC_LINE_LOG, ?base_logger_util:info_msg("#####~p:line:~p~n",[?MODULE,?LINE])).
% -define(FUNC_LINE_LOG, ok).

% -----------------------------------------------------
% release 配置
% -----------------------------------------------------