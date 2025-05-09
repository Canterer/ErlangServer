%% Description: TODO: Add description to base_gm_client_sup
-module(base_gm_client_sup).

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	start_link/1,
	get_client_count/0
]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_gen_sup_shared.hrl").

%% ====================================================================
%% External functions
%% ====================================================================
start_link([OnReceiveData,OnClientClose])->
	supervisor:start_link({local, ?SERVER},?MODULE, [OnReceiveData,OnClientClose]).

get_client_count()->
	%% count_children函数在新版的OTP库中存在
	%%supervisor:count_children(?MODULE).
	not_implement.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
?init([OnReceiveData,OnClientClose]) ->
	{ok,{{simple_one_for_one,5, 60}, 
		[
			{base_gm_client_statem, 				%% target process noname
			{base_gm_client_statem,start_link,[OnReceiveData,OnClientClose]},
			temporary, 				%% target process is temporary
			brutal_kill,worker,[]}
		]}}.

%% --------------------------------------------------------------------
%%% not export functions
%% --------------------------------------------------------------------
