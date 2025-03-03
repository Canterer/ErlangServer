-module(base_auth_gm_func_mod).

%%
%% Include files
%%
-include("login_pb.hrl").
%%
%% Exported Functions
%%
-export([
	validate_gm/6,
	validate_gm_test/6
]).

%%
%% API Functions
%%
validate_gm(GmName, GmId,Time,AuthResult,SecretKey,CfgTimeOut)->
	%%base_logger_util:msg("validate_gm ~p ~p ~p ~p ~p ~p~n",[GmName, GmId,Time,AuthResult,SecretKey,CfgTimeOut]),
	{MegaSec,Sec,_} = timer_center:get_correct_now(),
	Seconds = MegaSec*1000000 + Sec,
	DiffTim = erlang:abs(Seconds-Time),
	GmAccounts = env:get(gmaccount,[]),
	if DiffTim>CfgTimeOut->
			{error,200001};
		true ->
			ValStr = integer_to_list(GmId)
						++ auth_util:escape_uri(GmName) ++ integer_to_list(Time)
						++ SecretKey,
			MD5Bin = erlang:md5(ValStr),
			Md5Str = auth_util:binary_to_hexstring(MD5Bin),
			AuthStr = string:to_upper(AuthResult),
			SecretKeystr=string:to_upper(SecretKey),
			Ret = string:equal(AuthStr, SecretKeystr),%%@@string:equal(AuthStr, Md5Str)
			if Ret ->
					case lists:member(GmName,GmAccounts) of
						true->
							{ok,GmId};
						_->
							{error,20000}
					end;
				true->
					%%base_logger_util:msg("failed AuthResult =~p AuthStr=~p Md5Str=~p",[AuthResult,AuthStr,Md5Str]),
					{error,20000}
			end
	end.

validate_gm_test(_GmName, GmId,_Time,_AuthResult,_SecretKey,_CfgTimeOut)->
	base_logger_util:msg("validate_gm_test"),
	{ok,GmId}.
