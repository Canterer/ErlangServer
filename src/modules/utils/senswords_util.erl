%% Description: TODO: Add description to senswords
-module(senswords_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
	word_is_sensitive/1,
	import_words/2,
	replace_sensitive/1
]).

%%
%% API Functions
%%
%%
%% Include files
%%
-define(ETS_SENSITIVE,'$game_sensitive_words$').
-define(ETS_SENSITIVE_BLACKNAME,'$game_sensitive_words_blackname$').
-define(ETS_SENSITIVE_BLACKHEADER,'game_sensitive_words_blackheader$').

%% --------------------------------------------------------------------
%% behaviour include shared code
%% --------------------------------------------------------------------
-define(ETS_OPERATER_BEHAVIOUR,true).
% -define(DB_OPERATER_BEHAVIOUR,true).
-include("base_all_behaviour_shared.hrl").
%% --------------------------------------------------------------------
%%% behaviour functions begine
%% --------------------------------------------------------------------

%%
%% Exported Functions
%%

?create_ets()->
	?base_ets:new(?ETS_SENSITIVE, [named_table,set]),
	?base_ets:new(?ETS_SENSITIVE_BLACKNAME, [named_table,set]),
	?base_ets:new(?ETS_SENSITIVE_BLACKHEADER, [named_table,set]).
%%
%% API Functions
%%
?init_ets()->
	?base_ets:delete_all_objects(?ETS_SENSITIVE),
	?base_ets:delete_all_objects(?ETS_SENSITIVE_BLACKNAME),
	?base_ets:delete_all_objects(?ETS_SENSITIVE_BLACKHEADER),
	DictionaryFile = base_env_ets:get(sensitive,[]),
	BlackNameFile = base_env_ets:get(nameissensitive,[]),
	BlackHeaderFile = base_env_ets:get(nameisblackheader,[]),
	import_black_words(BlackNameFile,?ETS_SENSITIVE_BLACKNAME),
	import_words(BlackHeaderFile,?ETS_SENSITIVE_BLACKHEADER),
	add_word_to_ets(<<"游客">>,?ETS_SENSITIVE_BLACKHEADER),
	add_word_to_ets(<<"纯白">>,?ETS_SENSITIVE_BLACKHEADER),
	import_words(DictionaryFile,?ETS_SENSITIVE).

%%
%% Local Functions
%%

import_black_words(File,EtsName)->
	case file:consult(File) of
		{ok, [Terms]}-> 
			lists:foreach(fun(X)->
								  UniString = unicode:characters_to_list(X,unicode),
								  ?base_ets:insert(EtsName, {UniString})
						   end, Terms);
		{error,Reason}->
			base_logger_util:info_msg("import_black_words error:~p~n",[Reason])
	end.

import_words(File,EtsName)->
	case file:consult(File) of
		{ok, [Terms]}->
			lists:foreach(fun(X)->
								  add_word_to_ets(X,EtsName)
						  end,Terms);
		{error,Reason}->
			base_logger_util:info_msg("import_words error:~p~n",[Reason])
	end.
	

add_word_to_ets(Word,EtsName)->
	UniString = unicode:characters_to_list(Word,unicode),
	case UniString of
		[]-> ignor;
		_->
			[HeadChar|_Left] = UniString,
			case ?base_ets:lookup(EtsName, HeadChar) of
				[]-> ?base_ets:insert(EtsName, {HeadChar,[UniString]});
				[{_H,OldList}]->
					case lists:member(UniString,OldList) of
						false->?base_ets:insert(EtsName,{HeadChar,[UniString|OldList]});
						true-> ignor
					end
			end
	end.

word_is_sensitive([])->
	false;
word_is_sensitive(Utf8String) when is_list(Utf8String)->
	Utf8Binary = list_to_binary(Utf8String),
	word_is_sensitive(Utf8Binary);
word_is_sensitive(Utf8Binary) when is_binary(Utf8Binary)->
	UniString = unicode:characters_to_list(Utf8Binary,unicode),
	case word_is_sensitive_kernel(UniString) of
		true-> true;
		false->
		case match_blackheader_char_wordlist(UniString) of
			true-> true;
			false->match_black_char_wordlist(UniString)
		end
	end.
word_is_sensitive_kernel([])->
	false;
word_is_sensitive_kernel(UniString)->
	[HeadChar|TailString] = UniString,
	UniStrLen = length(UniString),
	WordList = get_key_char_wordlist(HeadChar),
	Match = fun(Word)->
					WordLen = length(Word),
					if WordLen> UniStrLen-> false; %%小于敏感词长度直接false
					   WordLen =:=	UniStrLen->	UniString =:= Word; %%等于直接比较
					   true-> %%大于取词比较
						   HeadStr = lists:sublist(UniString,WordLen),
						   HeadStr =:= Word
					end
			end,
	case lists:any(Match, WordList) of
		true-> true;
		false-> word_is_sensitive_kernel(TailString)
	end.
		
replace_sensitive(Utf8String) when is_binary(Utf8String)->
	UniString = unicode:characters_to_list(Utf8String,unicode),
	ReplacedString = replace_sensitive_kernel(UniString,[]),
	unicode:characters_to_binary(ReplacedString, utf8);
replace_sensitive(InputString)when is_list(InputString)->
	Utf8Binary = list_to_binary(InputString),
	replace_sensitive(Utf8Binary);
replace_sensitive(InputString)->
	InputString.

match_of_replace_sensitive_kernel(Word,Last,InputString,InputStrLen)->
	case Last of
		0->
		WordLen = length(Word),
		if WordLen>InputStrLen -> 0;
			WordLen=:=InputStrLen->
				if(InputString =:= Word)->
						WordLen;
				  true->
				  		0
				  end;
			true->
				HeadStr = lists:sublist(InputString,length(Word)),
				if(HeadStr =:= Word)->
					WordLen;
				  true->
				  	0
				  end
				end;
			_-> Last
		end.

replace_sensitive_kernel([],LastRepaced)->
	LastRepaced;
replace_sensitive_kernel(InputString,LastReplaced)->
	[HeadChar|TailString] = InputString,
	WordList = get_key_char_wordlist(HeadChar),
	InputStrLen = length(InputString),
	
	Match = fun(Word,Last)->
			match_of_replace_sensitive_kernel(Word,Last,InputString,InputStrLen)
			end,
			
	case lists:foldl(Match,0 ,WordList) of
		0-> 
		NewReplaced = LastReplaced ++ [HeadChar],
		replace_sensitive_kernel(TailString,NewReplaced);
		SensWordLen->
			LeftString = lists:sublist(InputString, SensWordLen + 1, InputStrLen - SensWordLen ),
			NewReplaced = LastReplaced ++ make_sensitive_show_string(SensWordLen),
			replace_sensitive_kernel(LeftString ,NewReplaced)
	end.

get_key_char_wordlist(KeyChar)->		
	case ?base_ets:lookup(?ETS_SENSITIVE,KeyChar) of
		[]-> [];
		[{_H,WordList}]-> WordList
	end.

match_black_char_wordlist(String)->
	case ?base_ets:lookup(?ETS_SENSITIVE_BLACKNAME,String) of
		[]-> false;
		_-> true
	end.


for_blackheader_any(Item_of_WordList,InputString)->
	WordLen = length(Item_of_WordList),
	InputHeader = string:substr(InputString,1,WordLen),
	if InputHeader=:=Item_of_WordList-> true;
		true-> false
	end.


match_blackheader_char_wordlist(InputString)->
	[HeadChar|_TailString] = InputString,
	BlackHeaderList = case ?base_ets:lookup(?ETS_SENSITIVE_BLACKHEADER,HeadChar) of
						[]-> [];
						[{_H,WordList}]->WordList
					  end,
	lists:any(fun(X)-> for_blackheader_any(X,InputString) end,BlackHeaderList).
	
	
make_sensitive_show_string(1)->
	"*";
make_sensitive_show_string(2)->
	"*&";
make_sensitive_show_string(3)->
	"*&^";
make_sensitive_show_string(4)->
	"*&^%";
make_sensitive_show_string(5)->
	"*&^%$";
make_sensitive_show_string(6)->
	"*&^%$#";
make_sensitive_show_string(7)->
	"*&^%$#@";
make_sensitive_show_string(8)->
	"*&^%$#@!";
make_sensitive_show_string(N)->
	M = N rem 8,
	C = N div 8,
	L1 = make_sensitive_show_string(M),
	L2 = lists:append(lists:duplicate(C,"*&^%$#@!")),
	lists:append([L2,L1]).

