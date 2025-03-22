-module(auth_qq_platform).

%%
%% Include files
%%
-include("user_auth.hrl").
%%
%% Exported Functions
%%
-export([
	validate_user/5,
	validate_user_test/5,
	validate_visitor/5,
	validate_visitor_test/5,
	make_key/3
]).
%%
%% API Functions
%%
validate_user(UserAuth,SecretKey,CfgTimeOut,FatigueList,NoFatigueList)->
	#user_auth{userid=UserId,openid=OpenId,openkey=OpenKey,appid=AppId,pf=Pf,userip=UserIp,lgtime=Time} = UserAuth,
%%	{MegaSec,Sec,_} = base_timer_server:get_correct_now(),
%%	Seconds = MegaSec*1000000 + Sec,
%%	DiffTim = erlang:abs(Seconds-list_to_integer(Time)),
%%	base_logger_util:info_msg("1111111~n"),
%%	if DiffTim>CfgTimeOut->
%%		   {error,timeout};			
%%		true ->
			case verify(OpenId,OpenKey,AppId,Pf,UserIp) of
				{ok, {NickName,Gender,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level}}->
					{ok,{NickName,Gender,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level}};
				D->
					{error,authentication_failure}
%%			end
	end.

validate_user_test(UserAuth,_SecretKey,_CfgTimeOut,_FatigueList,_NoFatigueList)->
	#user_auth{cm = Adult,userid = UserId} = UserAuth,
	case Adult of
		%%1->{ok,UserId,true};
		1->{ok,{"NAME","男",1,1,1}};
		_->{ok,{"NAME","男",1,1,1}}
	end.

validate_visitor(Time,AuthResult,VisitorKey,CfgTimeOut,NeedPlayerId)->
	{MegaSec,Sec,_} = base_timer_server:get_correct_now(),
	Seconds = MegaSec*1000000 + Sec,
	DiffTim = erlang:abs(Seconds-Time),
	if DiffTim>CfgTimeOut->
			{error,timeout};
		true ->
			ValStr = integer_to_list(Time)++ VisitorKey,
			MD5Bin = erlang:md5(ValStr),
			Md5Str = base_auth_util:binary_to_hexstring(MD5Bin),
			AuthStr = string:to_upper(AuthResult),
			Ret = string:equal(AuthStr, Md5Str),
			if
				Ret->
					{PlayerId,PlayerName} =case NeedPlayerId of
												true->do_genvistor();
												_-> {0,[]}
					end,
					{ok,{PlayerId,PlayerName},false};
				true->
					{error,authentication_failure}
			end
	end.

validate_visitor_test(_Time,_AuthResult,_VisitorKey,_CfgTimeOut,NeedPlayerId)->
	{PlayerId,PlayerName} =case NeedPlayerId of
								true->do_genvistor();
								_-> {0,[]}
	end,
	{ok,{PlayerId,PlayerName},false}.

%%
%%%%生成一个验证码
%%
make_key(UserName,Time,Adult)->
	BinName = case is_binary(UserName) of
						true-> UserName;
						_-> list_to_binary(UserName)
	end,
	NameEcode = base_auth_util:escape_uri(BinName),
	SecretKey = base_env_ets:get(platformkey, ""),
	ValStr = NameEcode ++ integer_to_list(Time)++ SecretKey ++ integer_to_list(Adult),
	MD5Bin = erlang:md5(ValStr),
	Md5Str = base_auth_util:binary_to_hexstring(MD5Bin),
	ValStr.

%%
%% Local Functions
%%
verify(OpenId,OpenKey,_,Pf,UserIp)->
	Host = base_env_ets:get(id_secret_host,[]),
	Port = base_env_ets:get(id_secret_port,[]),
	AppId = base_env_ets:get(id_secret_appid,[]),
	Sig = make_sig(AppId,OpenId,OpenKey,Pf,UserIp),
	DataStr = get_data_string(OpenId,OpenKey,AppId,Sig,Pf,UserIp),
	% DataLength = erlang:length(DataStr),
	SendUrl = "GET /v3/user/get_info" ++ "?" ++ DataStr ++ " " ++  "HTTP/1.1\r\nHost:" ++ Host
				++"\r\n\r\n",

	Code = 
	try
		Socket = case gen_tcp:connect(Host, Port, [{packet,0},binary,{active, true}]) of
						{ok,S}->S;
						{error,R1}->exit(R1)
		end,
		case gen_tcp:send(Socket,SendUrl) of
				ok ->next;
				{error,R2}->
					base_logger_util:info_msg("gen_tcp:close ~p:line:~p !!!~n",[?MODULE,?LINE]),
					gen_tcp:close(Socket),
					exit(R2)
		end,
		RecvPacket = 
		receive
			{tcp,_,P}-> 
				P,
				Result = binary_to_list(P),
				base_logger_util:info_msg("json:~s~n", [Result]),
				{_,JsonObj} = base_json_util:json_decode("{" ++ lists:last(string:tokens(Result,"{"))),
				handle_json(JsonObj);
			_->
				{error,tcp_connect_error}
		end,
		base_logger_util:info_msg("gen_tcp:close ~p:line:~p !!!~n",[?MODULE,?LINE]),
		gen_tcp:close(Socket),
		RecvPacket 	 		
	catch
		R:E->
			base_logger_util:info_msg("R:~p,E:~p~n",[R,E]), 
			{error,E}
	end.

make_sig(AppId,OpenId,OpenKey,Pf,UserIp)->
	Urlencode = base_url_util:urlencode("/v3/user/get_info"),
	Dataencode =  base_url_util:urlencode("appid="++AppId++"&openid="++OpenId++"&openkey="++OpenKey++"&pf="++Pf),
	BaseString = "GET&"++Urlencode++"&"++Dataencode,
	AppKey = base_env_ets:get(id_secret_appkey,"8256306d3d287ac69c7632513a75aa54"),
	base64:encode_to_string(crypto:hmac(sha, AppKey ++ "&", BaseString)).

handle_json({struct,_} = JsonObj)->
	Ret = base_json_util:get_json_member(JsonObj,"ret"),
	case Ret of
		{ok,0}-> 
			case base_json_util:get_json_member(JsonObj,"nickname") of
				{ok,NickName}->
					NickName;
				_->
					NickName = []
			end,
			case base_json_util:get_json_member(JsonObj,"gender") of
				{ok,Gender}->
					Gender;
				_->
					Gender = []
			end,
			case base_json_util:get_json_member(JsonObj,"is_yellow_vip") of
				{ok,Is_yellow_vip}->
					Is_yellow_vip;
				_->
					Is_yellow_vip = []
			end,
			case base_json_util:get_json_member(JsonObj,"is_yellow_year_vip") of
				{ok,Is_yellow_year_vip}->
					Is_yellow_year_vip;
				_->
					Is_yellow_year_vip = []
			end,
			case base_json_util:get_json_member(JsonObj,"yellow_vip_level") of
				{ok,Yellow_vip_level}->
					Yellow_vip_level;
				_->
					Yellow_vip_level = []
			end,
			{ok,{NickName,Gender,Is_yellow_vip,Is_yellow_year_vip,Yellow_vip_level}};
		R->
			{error,R}
	end.

get_data_string(OpenId,OpenKey,AppId,Sig,Pf,UserIp)->
	"openid="++ OpenId ++ "&openkey=" ++ OpenKey ++ "&appid=" ++ AppId ++ "&sig="
		++ base_url_util:urlencode(Sig) ++ "&pf=" ++ Pf.

check_fatigue(AccountName,OldAdultFlag,FatigueList,NoFatigueList)->
	case lists:filter(fun({Account,_})->
						Account=:=AccountName
						end, FatigueList) of
		[]-> 
			case lists:filter(fun({Account,_})->
								Account=:=AccountName
								end, NoFatigueList) of
				[]->OldAdultFlag;
				[{_Account,_Level}]-> 1;
				[{_Account,_Level}|_T] -> 1
			end;
		[{_Account,_Level}]->0;
		[{_Account,_Level}|_T]->0
	end.

do_genvistor()->
	Id = visitor_generator:gen_newid(),
	{Id,"##visitor##_" ++ integer_to_list(Id)}.
