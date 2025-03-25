%%% Description : temp function

-module(base_temp_util).
-include("common_define.hrl").
-define(MAX_ROLE_COUNT,4294967295).	

-export([
	make_int_str/1,
	make_int_str2/1,
	make_int_str3/1,
	make_int_str4/1,
	make_int_str5/1,
	make_int_str6/1,
	make_int_str7/1,
	make_int_str8/1,
	make_int_str20/1,
	make_int_str30/1,
	get_serverid_by_roleid/1,
	term_to_record/2,
	get_min_count_of_lines/1
]).

make_int_str(Int)->
	integer_to_list(Int).

make_int_str2(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0", Str);
		_-> Str
	end.

make_int_str3(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("00", Str);
		2-> string:concat("0", Str);
		_-> Str
	end.

make_int_str4(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("000", Str);
		2-> string:concat("00", Str);
		3-> string:concat("0", Str);
		_-> Str
	end.

make_int_str5(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0000", Str);
		2-> string:concat("000", Str);
		3-> string:concat("00", Str);
		4-> string:concat("0", Str);
		_-> Str
	end.

make_int_str6(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("00000", Str);
		2-> string:concat("0000", Str);
		3-> string:concat("000", Str);
		4-> string:concat("00", Str);
		5-> string:concat("0", Str);
		_-> Str
	end.

make_int_str7(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("000000", Str);
		2-> string:concat("00000", Str);
		3-> string:concat("0000", Str);
		4-> string:concat("000", Str);
		5-> string:concat("00", Str);
		6-> string:concat("0", Str);
		_-> Str
	end.	

make_int_str8(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0000000", Str);
		2-> string:concat("000000", Str);
		3-> string:concat("00000", Str);
		4-> string:concat("0000", Str);
		5-> string:concat("000", Str);
		6-> string:concat("00", Str);
		7-> string:concat("0", Str);
		_-> Str
	end.

make_int_str20(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0000000000000000000", Str);
		2-> string:concat("000000000000000000", Str);
		3-> string:concat("00000000000000000", Str);
		4-> string:concat("0000000000000000", Str);
		5-> string:concat("000000000000000", Str);
		6-> string:concat("00000000000000", Str);
		7-> string:concat("0000000000000", Str);
		8-> string:concat("000000000000", Str);
		9-> string:concat("00000000000", Str);
		10-> string:concat("0000000000", Str);
		11-> string:concat("000000000", Str);
		12-> string:concat("00000000", Str);
		13-> string:concat("0000000", Str);
		14-> string:concat("000000", Str);
		15-> string:concat("00000", Str);
		16-> string:concat("0000", Str);
		17-> string:concat("000", Str);
		18-> string:concat("00", Str);
		19-> string:concat("0", Str);
		_-> Str
	end.

make_int_str30(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("00000000000000000000000000000", Str);
		2-> string:concat("0000000000000000000000000000", Str);
		3-> string:concat("000000000000000000000000000", Str);
		4-> string:concat("00000000000000000000000000", Str);
		5-> string:concat("0000000000000000000000000", Str);
		6-> string:concat("000000000000000000000000", Str);
		7-> string:concat("00000000000000000000000", Str);
		8-> string:concat("0000000000000000000000", Str);
		9-> string:concat("000000000000000000000", Str);
		10-> string:concat("00000000000000000000", Str);
		11-> string:concat("0000000000000000000", Str);
		12-> string:concat("000000000000000000", Str);
		13-> string:concat("00000000000000000", Str);
		14-> string:concat("0000000000000000", Str);
		15-> string:concat("000000000000000", Str);
		16-> string:concat("00000000000000", Str);
		17-> string:concat("0000000000000", Str);
		18-> string:concat("000000000000", Str);
		19-> string:concat("00000000000", Str);
		20-> string:concat("0000000000", Str);
		21-> string:concat("000000000", Str);
		22-> string:concat("00000000", Str);
		23-> string:concat("0000000", Str);
		24-> string:concat("000000", Str);
		25-> string:concat("00000", Str);
		26-> string:concat("0000", Str);
		27-> string:concat("000", Str);
		28-> string:concat("00", Str);
		29-> string:concat("0", Str);
		_-> Str
	end.

get_serverid_by_roleid(RoleId)->
	trunc(RoleId / ?SERVER_MAX_ROLE_NUMBER).

term_to_record(Term,RecordName) ->
	list_to_tuple([RecordName | tuple_to_list(Term)]).

get_min_count_of_lines(LineInfos)->
	MinLineInfo = lists:foldl(fun(X,Min)-> 
						  {_,RoleCount}=X,
						  {_,MinRoleCount}=Min,
						  if
							  RoleCount < MinRoleCount -> X;
							  true  -> Min
						  end
				  end,
				  {0,?MAX_ROLE_COUNT}, LineInfos),
	MinLineInfo.