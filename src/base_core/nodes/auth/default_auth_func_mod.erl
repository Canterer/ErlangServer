-module(base_auth_func_mod).

%%
%% Include files
%%
-include("login_pb.hrl").
-include("user_auth.hrl").
%%
%% Exported Functions
%%
-export([
	validate_user/5,
	validate_user_test/5,
	validate_visitor/5,
	validate_visitor_test/5
]).
%%
%% API Functions
%%
validate_user(UserAuth,SecretKey,CfgTimeOut,FatigueList,NoFatigueList)->
	#user_auth{username=UserName,userid=UserId,lgtime=Time,cm=TmpAdult,flag=AuthResult} = UserAuth,
	Adult = list_to_integer(TmpAdult),
	{MegaSec,Sec,_} = base_timer_server:get_correct_now(),
	Seconds = MegaSec*1000000 + Sec,
	DiffTim = erlang:abs(Seconds-list_to_integer(Time)),
	if DiffTim>CfgTimeOut->
			{error,timeout};
		true ->
			BinName = case is_binary(UserName) of
							true-> UserName;
							_-> list_to_binary(UserName)
			end,
			NameEcode = base_auth_util:escape_uri(BinName),
			ValStr = UserId
						++ NameEcode ++ Time
						++ SecretKey ++ TmpAdult,
			MD5Bin = erlang:md5(ValStr),
			Md5Str = base_auth_util:binary_to_hexstring(MD5Bin),
			AuthStr = string:to_upper(AuthResult),
			Ret = string:equal(AuthStr, Md5Str),
			if Ret ->
				case check_fatigue(UserName,Adult,FatigueList,NoFatigueList) of
					1->{ok,UserId,true};
					_->{ok,UserId,false}
				end;
			true->
				{error,authentication_failure}
			end
	end.

validate_user_test(UserAuth,_SecretKey,_CfgTimeOut,_FatigueList,_NoFatigueList)->
	#user_auth{cm = Adult,userid = UserId} = UserAuth,
	case Adult of
		1->{ok,UserId,false};
		_->{ok,UserId,false}
	end.

do_genvistor()->
	Id = visitor_generator:gen_newid(),
	{Id,"##visitor##_" ++ integer_to_list(Id)}.

check_fatigue(AccountName,OldAdultFlag,FatigueList,NoFatigueList)->
	case lists:filter(fun({Account,_})->
							Account=:=AccountName
						end, FatigueList ) of
		[]-> 
			case lists:filter(fun({Account,_})->
									Account=:=AccountName
								end , NoFatigueList ) of
				[]->OldAdultFlag;
				[{_Account,_Level}]-> 1;
				[{_Account,_Level}|_T] -> 1
			end;
		[{_Account,_Level}]->0;
		[{_Account,_Level}|_T]->0
	end.