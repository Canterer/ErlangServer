-module(mmake).
-export([all/1, all/2, files/2, files/3]).

-include_lib("kernel/include/file.hrl").
% file.hrl 内含#file_info记录的定义

-define(MakeOpts,[noexec,load,netload,noload]).

all(Worker) when is_integer(Worker) ->
	all(Worker, []).

all(Worker, Options) when is_integer(Worker) ->
	{MakeOpts, CompileOpts} = sort_options(Options,[],[]),
	case read_emakefile('Emakefile', CompileOpts) of
		Files when is_list(Files) ->
			do_make_files(Worker, Files, MakeOpts);
		error ->
			error
	end.

files(Worker, Fs) ->
	files(Worker, Fs, []).

files(Worker, Fs0, Options) ->
	Fs = [filename:rootname(F,".erl") || F <- Fs0],
	{MakeOpts,CompileOpts} = sort_options(Options,[],[]),
	case get_opts_from_emakefile(Fs,'Emakefile',CompileOpts) of
	Files when is_list(Files) ->
		do_make_files(Worker, Files,MakeOpts);		
	error -> error
	end.

do_make_files(Worker, Fs, Opts) ->
	process(Fs, Worker, lists:member(noexec, Opts), load_opt(Opts)).

sort_options([H|T],Make,Comp) ->
	case lists:member(H,?MakeOpts) of
	true ->
		sort_options(T,[H|Make],Comp);
	false ->
		sort_options(T,Make,[H|Comp])
	end;
sort_options([],Make,Comp) ->
	{Make,lists:reverse(Comp)}.

%%% Reads the given Emakefile and returns a list of tuples: {Mods,Opts}
%%% Mods is a list of module names (strings)
%%% Opts is a list of options to be used when compiling Mods
%%%
%%% Emakefile can contain elements like this:
%%% Mod.
%%% {Mod,Opts}.
%%% Mod is a module name which might include '*' as wildcard
%%% or a list of such module names
%%%
%%% These elements are converted to [{ModList,OptList},...]
%%% ModList is a list of modulenames (strings)
read_emakefile(Emakefile,Opts) ->
	case file:consult(Emakefile) of
	{ok, Emake} ->
		compile_app(Emake),
		transform(Emake,Opts,[],[]);
	{error,enoent} ->
		%% No Emakefile found - return all modules in current 
		%% directory and the options given at command line
		Mods = [filename:rootname(F) ||  F <- filelib:wildcard("*.erl")],
		[{Mods, Opts}];
	{error,Other} ->
		base_logger_util:info_msg("make: Trouble reading 'Emakefile':~n~p~n",[Other]),
		error
	end.

transform([{Mod,ModOpts}|Emake],Opts,Files,Already) ->
	case expand(Mod,Already) of
	[] -> 
		transform(Emake,Opts,Files,Already);
	Mods -> 
		transform(Emake,Opts,[{Mods,ModOpts++Opts}|Files],Mods++Already)
	end;
transform([Mod|Emake],Opts,Files,Already) ->
	case expand(Mod,Already) of
	[] -> 
		transform(Emake,Opts,Files,Already);
	Mods ->
		transform(Emake,Opts,[{Mods,Opts}|Files],Mods++Already)
	end;
transform([],_Opts,Files,_Already) ->
	lists:reverse(Files).

expand(Mod,Already) when is_atom(Mod) ->
	expand(atom_to_list(Mod),Already);
expand(Mods,Already) when is_list(Mods), not is_integer(hd(Mods)) ->
	lists:concat([expand(Mod,Already) || Mod <- Mods]);
expand(Mod,Already) ->
	case lists:member($*,Mod) of
	true -> 
		Fun = fun(F,Acc) -> 
			  M = filename:rootname(F),
			  case lists:member(M,Already) of
				  true -> Acc;
				  false -> [M|Acc]
			  end
		  end,
		lists:foldl(Fun, [], filelib:wildcard(Mod++".erl"));
	false ->
		Mod2 = filename:rootname(Mod, ".erl"),
		case lists:member(Mod2,Already) of
		true -> [];
		false -> [Mod2]
		end
	end.

%%% Reads the given Emakefile to see if there are any specific compile 
%%% options given for the modules.
get_opts_from_emakefile(Mods,Emakefile,Opts) ->
	case file:consult(Emakefile) of
	{ok,Emake} ->
		Modsandopts = transform(Emake,Opts,[],[]),
		ModStrings = [coerce_2_list(M) || M <- Mods],
		get_opts_from_emakefile2(Modsandopts,ModStrings,Opts,[]); 
	{error,enoent} ->
		[{Mods, Opts}];
	{error,Other} ->
		base_logger_util:info_msg("make: Trouble reading 'Emakefile':~n~p~n",[Other]),
		error
	end.

get_opts_from_emakefile2([{MakefileMods,O}|Rest],Mods,Opts,Result) ->
	case members(Mods,MakefileMods,[],Mods) of
	{[],_} -> 
		get_opts_from_emakefile2(Rest,Mods,Opts,Result);
	{I,RestOfMods} ->
		get_opts_from_emakefile2(Rest,RestOfMods,Opts,[{I,O}|Result])
	end;
get_opts_from_emakefile2([],[],_Opts,Result) ->
	Result;
get_opts_from_emakefile2([],RestOfMods,Opts,Result) ->
	[{RestOfMods,Opts}|Result].
	
members([H|T],MakefileMods,I,Rest) ->
	case lists:member(H,MakefileMods) of
	true ->
		members(T,MakefileMods,[H|I],lists:delete(H,Rest));
	false ->
		members(T,MakefileMods,I,Rest)
	end;
members([],_MakefileMods,I,Rest) ->
	{I,Rest}.


%% Any flags that are not recognixed as make flags are passed directly
%% to the compiler.
%% So for example make:all([load,debug_info]) will make everything
%% with the debug_info flag and load it.
load_opt(Opts) ->
	case lists:member(netload,Opts) of
	true -> 
		netload;
	false ->
		case lists:member(load,Opts) of
		true ->
			load;
		_ ->
			noload
		end
	end.

%% 
process([{[], _Opts}|Rest], Worker, NoExec, Load) ->
	process(Rest, Worker, NoExec, Load);
process([{L, Opts}|Rest], Worker, NoExec, Load) ->
	Len = length(L),
	Worker2 = erlang:min(Len, Worker),
	case catch do_worker(L, Opts, NoExec, Load, Worker2) of
		error ->
			error;
		ok ->
			process(Rest, Worker, NoExec, Load)
	end;
process([], _Worker, _NoExec, _Load) ->
	up_to_date.

%% use worker compile
do_worker(L, Opts, NoExec, Load, Worker) ->
	WorkerList = do_split_list(L, Worker),
	%base_logger_util:info_msg("worker:~p worker list(~p)~n", [Worker, length(WorkerList)]),
	% å¯åŠ¨è¿›ç¨‹
	Ref = make_ref(),
	Pids =
	[begin
		start_worker(E, Opts, NoExec, Load, self(), Ref)
	end || E <- WorkerList],
	do_wait_worker(length(Pids), Ref).

%% wait result
do_wait_worker(0, _Ref) ->
	ok;
do_wait_worker(N, Ref) ->
	receive
		{ack, Ref} ->
			do_wait_worker(N - 1, Ref);
		{error, Ref} ->
			throw(error);
		{'EXIT', _P, _Reason} ->
			do_wait_worker(N, Ref);
		_Other ->
			base_logger_util:info_msg("receive unknown msg:~p~n", [_Other]),
			do_wait_worker(N, Ref)
	end.

%% å°†Låˆ†å‰²æˆæœ€å¤šåŒ…å«Nä¸ªå­åˆ—è¡¨çš„åˆ—è¡¨
do_split_list(L, N) ->
	Len = length(L), 
	% æ¯ä¸ªåˆ—è¡¨çš„å…ƒç´ æ•°
	LLen = (Len + N - 1) div N,
	do_split_list(L, LLen, []).

do_split_list([], _N, Acc) ->
	lists:reverse(Acc);
do_split_list(L, N, Acc) ->
	{L2, L3} = lists:split(erlang:min(length(L), N), L),
	do_split_list(L3, N, [L2 | Acc]).

%% start worker processor
start_worker(L, Opts, NoExec, Load, Parent, Ref) ->
	Fun = 
	fun() ->
		[begin 
			case recompilep(coerce_2_list(F), NoExec, Load, Opts) of
				error ->
					Parent ! {error, Ref},
					exit(error);
				_ ->
					ok
			end
		end || F <- L],
		Parent ! {ack, Ref}
	end,
	spawn_link(Fun).

recompilep(File, NoExec, Load, Opts) ->
	ObjName = lists:append(filename:basename(File),
			   code:objfile_extension()),
	ObjFile = case lists:keysearch(outdir,1,Opts) of
		  {value,{outdir,OutDir}} ->
			  filename:join(coerce_2_list(OutDir),ObjName);
		  false ->
			  ObjName
		  end,
	case exists(ObjFile) of
	true ->
		recompilep1(File, NoExec, Load, Opts, ObjFile);
	false ->
		recompile(File, NoExec, Load, Opts)
	end.
 
recompilep1(File, NoExec, Load, Opts, ObjFile) ->
	{ok, Erl} = file:read_file_info(lists:append(File, ".erl")),
	{ok, Obj} = file:read_file_info(ObjFile),
	 recompilep1(Erl, Obj, File, NoExec, Load, Opts).

recompilep1(#file_info{mtime=Te},
		#file_info{mtime=To}, File, NoExec, Load, Opts) when Te>To ->
	recompile(File, NoExec, Load, Opts);
recompilep1(_Erl, #file_info{mtime=To}, File, NoExec, Load, Opts) ->
	recompile2(To, File, NoExec, Load, Opts).

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
recompile2(ObjMTime, File, NoExec, Load, Opts) ->
	IncludePath = include_opt(Opts),
	case check_includes(lists:append(File, ".erl"), IncludePath, ObjMTime) of
	true ->
		recompile(File, NoExec, Load, Opts);
	false ->
		false
	end.

include_opt([{i,Path}|Rest]) ->
	[Path|include_opt(Rest)];
include_opt([_First|Rest]) ->
	include_opt(Rest);
include_opt([]) ->
	[].

%% recompile(File, NoExec, Load, Opts)
%% Actually recompile and load the file, depending on the flags.
%% Where load can be netload | load | noload

recompile(File, true, _Load, _Opts) ->
	base_logger_util:info_msg("Out of date: ~s\n",[File]);
recompile(File, false, noload, Opts) ->
	base_logger_util:info_msg("Recompile: ~s\n",[File]),
	compile:file(File, [report_errors, report_warnings, error_summary |Opts]);
recompile(File, false, load, Opts) ->
	base_logger_util:info_msg("Recompile: ~s\n",[File]),
	c:c(File, Opts);
recompile(File, false, netload, Opts) ->
	base_logger_util:info_msg("Recompile: ~s\n",[File]),
	c:nc(File, Opts).

exists(File) ->
	case file:read_file_info(File) of
	{ok, _} ->
		true;
	_ ->
		false
	end.

coerce_2_list(X) when is_atom(X) ->
	atom_to_list(X);
coerce_2_list(X) ->
	X.

%%% If you an include file is found with a modification
%%% time larger than the modification time of the object
%%% file, return true. Otherwise return false.
check_includes(File, IncludePath, ObjMTime) ->
	Path = [filename:dirname(File)|IncludePath], 
	case epp:open(File, Path, []) of
	{ok, Epp} ->
		check_includes2(Epp, File, ObjMTime);
	_Error ->
		false
	end.
	
check_includes2(Epp, File, ObjMTime) ->
	case epp:parse_erl_form(Epp) of
	{ok, {attribute, 1, file, {File, 1}}} ->
		check_includes2(Epp, File, ObjMTime);
	{ok, {attribute, 1, file, {IncFile, 1}}} ->
		case file:read_file_info(IncFile) of
		{ok, #file_info{mtime=MTime}} when MTime>ObjMTime ->
			epp:close(Epp),
			true;
		_ ->
			check_includes2(Epp, File, ObjMTime)
		end;
	{ok, _} ->
		check_includes2(Epp, File, ObjMTime);
	{eof, _} ->
		epp:close(Epp),
		false;
	{error, _Error} ->
		check_includes2(Epp, File, ObjMTime)
	end.

compile_app(Files) when is_list(Files)->
	lists:foreach(fun(File)-> compile_app(File) end, Files);
compile_app({File,Options}) when is_atom(File)->
	compile_app(atom_to_list(File),Options);
compile_app({Files,Options}) when is_list(Files)->
	lists:foreach(fun(File)->
						  compile_app(atom_to_list(File),Options)
				  end, Files).

compile_app(File,Options)->
	AppFiles = filelib:wildcard(File ++ ".app.src"),
	case lists:keyfind(outdir, 1, Options) of
		{_,OutDir}-> 
			AppOptions = lists:map(fun(OrgFile)->
										   {OrgFile,OutDir++ "/" ++filename:basename(OrgFile, ".src")}
								   end, AppFiles),
			
			lists:foreach(fun({OrgFile,AppFile})-> copy_app(OrgFile,AppFile) end, AppOptions);
		false-> o
	end.	

get_file_md5(File)->
	case file:read_file(File) of
		{ok,Binary}->erlang:md5(Binary);
		_-> <<>>
	end.

copy_app(SrcFile,DstFile)->
	SrcMd5 = get_file_md5(SrcFile),
	DstMd5 = get_file_md5(DstFile),
	if SrcMd5 =:= DstMd5-> 
		   ignor;
	   true->
		   case file:copy(SrcFile, DstFile) of
			   {ok,_}->
				   base_logger_util:info_msg("copy :~s\n",[DstFile]);
			   _->
				   base_logger_util:info_msg("copy fail:~s\n",[DstFile])
		   end
	end.
