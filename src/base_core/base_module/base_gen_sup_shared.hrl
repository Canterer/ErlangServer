%% Description: 本文件内含函数定义, include时需放在其他属性定义的最后
-include("base_define_shared.hrl").

% 自定义本模块宏定义 开启间接接口
% -undef(init).
% -undef(log_init).
% -define(init, log_init).
% -define(log_init, init).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).
-behaviour(supervisor).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%		  ignore						  |
%%		  {error, Reason}
%% --------------------------------------------------------------------
?log_init(Args)->
	?OTP_FUNC_START("Args=~p",[Args]),
	Returns = ?init(Args),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.