-module(base_json_util).
-export([
	json_encode/1,
	json_decode/1,
	get_json_member/2,
	get_json_member_pure/2
]).

json_encode({struct,_MemberList}=Term)->
	try
		Json = json:encode(Term),
		{ok,list_to_binary(Json)}
		%%{ok,term_to_binary(Term)}
	catch
		E:R-> 
			slogger:msg("json_encode exception ~p:~p~n~p",[E,R,Term]),
			{error,"Excption!"}
	end;
json_encode(S) when is_binary(S)->
	try
		Json = json:encode(S),
		{ok,list_to_binary(Json)}
	catch
		E:R-> 
			slogger:msg("s_encode exception ~p:~p",[E,R]),
			{error,"Excption!"}
	end;
json_encode(_)->
	{error,"not support!"}.

json_decode(Json) when is_list(Json)->
	try
		Term = json:decode(Json),
		{ok,Term}
	catch
		E:R-> slogger:msg("json_decode exception ~p:~p",[E,R])
	end;
json_decode(Json) when is_binary(Json)->
	try
		Term = json:decode(binary_to_list(Json)),
		{ok,Term}
	catch
		E:R-> slogger:msg("json_decode exception ~p:~p",[E,R])
	end;
json_decode(_)->
	{error}.
	


get_json_member(JsonObj,Member) when is_list(Member)->
	get_json_member_pure(JsonObj,list_to_binary(Member));
get_json_member(JsonObj,Member)when is_binary(Member)->
	get_json_member_pure(JsonObj,Member);
get_json_member(_JsonObj,_Member)->
	{error,"bad arguments"}.

get_json_member_pure(JsonObj,Member)->
	case JsonObj of
		{struct,MemberList}-> 
			case lists:keyfind(Member, 1, MemberList) of
				false-> {error,"cannot find"};
				{_,Value}-> 
					if is_binary(Value)->
						   {ok,binary_to_list(Value)};
					   true->
						   {ok,Value}
					end
			end;
		_-> {error,"bad json"}
	end.