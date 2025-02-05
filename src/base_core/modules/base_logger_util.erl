-module(base_logger_util).
%%
%% Include files
%%
-include("base_define_shared.hrl").
%%
%% Exported Functions
%%
-export([
	msg/1,msg/2,
	msg_filter/3,
	otp_func_start/2,
	otp_func_end/2,
	db_operater_start/2,
	db_operater_end/0,
	ets_operater_start/2,
	ets_operater_end/2
]).

%%
%% API Functions
%%
msg(Format)->
	error_logger:info_msg(Format).

msg(Format, Data)->
	error_logger:info_msg(Format, Data).

msg_filter(Id,Format,Data)->
	if Id== 2030096->
		   msg(Format,Data);
	   true->
		   ok
	end.

error_msg(Format)->
	error_logger:error_msg(Format).

error_msg(Format, Data)->
	error_logger:error_msg(Format, Data).


otp_func_start(Format, Data)->
	?OTP_FUNC_START(Format, Data).
otp_func_end(Format, Data)->
	?OTP_FUNC_END(Format, Data).

db_operater_start(Format, Data)->
	?DB_OPERATER_START(Format, Data).
db_operater_end()->
	?DB_OPERATER_END().

ets_operater_start(Format, Data)->
	?DB_OPERATER_START(Format, Data).
ets_operater_end(Format, Data)->
	?DB_OPERATER_START(Format, Data).

%%
%% Local Functions
%%