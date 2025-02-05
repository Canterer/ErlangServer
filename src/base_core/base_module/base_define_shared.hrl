%% Description: base define shared
-define(SERVER, ?MODULE).
-define(OTP_FUNC_START(Format, Data), base_logger_util:otp_func_start(Format, Data)).
-define(OTP_FUNC_END(Format, Data), base_logger_util:otp_func_end(Format, Data)).
% -define(OTP_FUNC_START(Format, Data), ok).
% -define(OTP_FUNC_END(Format, Data), ok).