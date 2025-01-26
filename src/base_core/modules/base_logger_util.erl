-module(base_logger_util).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([msg/1,msg/2,msg_filter/3]).
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
%%
%% Local Functions
%%