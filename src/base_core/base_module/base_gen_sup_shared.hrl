%% Description: 本文件内含函数定义, include时需放在其他属性定义的最后
-include("base_define_shared.hrl").

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%		  ignore						  |
%%		  {error, Reason}
%% --------------------------------------------------------------------
init(Args)->
	?OTP_FUNC_START("Args=~p",[Args]),
	Returns = do_init(Args),
	?OTP_FUNC_END("Returns=~p",[Returns]),
	Returns.