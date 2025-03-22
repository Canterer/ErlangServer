-module(base_logger_util).
%%
%% Include files
%%
-include("base_define_shared.hrl").
%%
%% Exported Functions
%%
-export([
	info_msg/1,
	info_msg/2,
	error_msg/1,
	error_msg/2
	% msg_filter/3
]).

%%
%% API Functions
%%
info_msg(Format)->
	error_logger:info_msg(Format).

info_msg(Format, Data)->
	error_logger:info_msg(Format, Data).

% msg_filter(Id,Format,Data)->
% 	if Id== 2030096->
% 		   msg(Format,Data);
% 	   true->
% 		   ok
% 	end.

error_msg(Format)->
	error_logger:error_msg(Format).

error_msg(Format, Data)->
	error_logger:error_msg(Format, Data).

%%
%% Local Functions
%%