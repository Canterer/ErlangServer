-module(base_mnesia).

-include("base_define_shared.hrl").

-export([
	%% Start, stop and debugging
	start/0,start/1,
	stop/0,
	% change_config/2,

	%% Activity mgt
	abort/1,
	transaction/1,transaction/2,transaction/3,
	% sync_transaction/1,sync_transaction/2,sync_transaction/3,
	% async_dirty/1,async_dirty/2,
	% sync_dirty/1,sync_dirty/2,
	ets/1,ets/2,
	activity/2,activity/3,activity/4,% Not for public use
	% is_transaction/0,

	%% Access within an activity - Lock acquisition
	% lock/2,lock/4,
	% lock_table/2,
	% read_lock_table/1,
	% write_lock_table/1,

	%% Access within an activity - Updates
	write/1,write/3,write/5,
	% s_write/1,
	delete/1,delete/3,delete/5,
	% s_delete/1,
	delete_object/1,delete_object/3,delete_object/5,
	% s_delete_object/1,

	%% Access within an activity - Reads
	read/1,read/2,read/3,read/5,
	% wread/1,
	match_object/1,match_object/3,match_object/5,
	% select/1,select/2,select/3,select/4,select/5,select/6,
	% all_keys/1,all_keys/4,
	% index_match_object/2,index_match_object/4,index_match_object/6,
	index_read/3,index_read/6,
	% first/1,next/2,last/1,prev/2,
	% first/3,next/4,last/3,prev/4,

	%% Iterators within an activity
	foldl/3,foldl/4,foldr/3,foldr/4,

	%% Dirty access regardless of activities - Updates
	dirty_write/1,dirty_write/2,
	dirty_delete/1,dirty_delete/2,
	dirty_delete_object/1,dirty_delete_object/2,
	% dirty_update_counter/2,dirty_update_counter/3,

	%% Dirty access regardless of activities - Read
	dirty_read/1,dirty_read/2,
	% dirty_select/2,
	% dirty_match_object/1,dirty_match_object/2,dirty_all_keys/1,
	dirty_index_match_object/2,dirty_index_match_object/3,
	dirty_index_read/3,
	% dirty_slot/2,
	% dirty_first/1,dirty_next/2,dirty_last/1,dirty_prev/2,

	%% Info
	table_info/2,table_info/4,
	% schema/0,schema/1,
	% error_description/1,
	% info/0,
	system_info/1,
	% system_info/0,                     % Not for public use

	%% Database mgt
	create_schema/1,create_schema/2,
	delete_schema/1,
    % add_backend_type/2,
	backup/1,backup/2,
	traverse_backup/4,traverse_backup/6,
	% install_fallback/1,install_fallback/2,
	% uninstall_fallback/0,uninstall_fallback/1,
	% activate_checkpoint/1,deactivate_checkpoint/1,
	% backup_checkpoint/2,backup_checkpoint/3,
	% restore/2,

	%% Table mgt
	create_table/1,create_table/2,
	delete_table/1,
	add_table_copy/3,del_table_copy/2,move_table_copy/3,
	% add_table_index/2,del_table_index/2,
	% transform_table/3,transform_table/4,
	change_table_copy_type/3,change_table_majority/2,
	% read_table_property/2,write_table_property/2,delete_table_property/2,
	% change_table_frag/2,
	clear_table/1,clear_table/4,

	%% Table load
	wait_for_tables/2,
	% dump_tables/1,force_load_table/1,
	% change_table_access_mode/2,change_table_load_order/2,
	% set_master_nodes/1,set_master_nodes/2,

	%% Misc admin
	% dump_log/0,sync_log/0,
	% subscribe/1,unsubscribe/1,report_event/1,

	%% Snmp
	% snmp_open_table/2,snmp_close_table/1,
	% snmp_get_row/2,snmp_get_next_index/2,snmp_get_mnesia_key/2,

	%% Textfile access
	% load_textfile/1,dump_to_textfile/1,

	%% Mnemosyne exclusive
	% get_activity_id/0,put_activity_id/1,% Not for public use

	%% Mnesia internal functions
	% dirty_rpc/4,                         % Not for public use
	% has_var/1,fun_select/7,fun_select/10,select_cont/3,dirty_sel_init/5,
	foldl/6,foldr/6,

	%% Module internal callback functions
	% raw_table_info/2,                     % Not for public use
	% remote_dirty_match_object/2,          % Not for public use
	% remote_dirty_select/2                  % Not for public use

	%% QLC functions
	table/1,table/2
]).


start()->
	mnesia:start().

start(ExtraEnv)->
	mnesia:start(ExtraEnv).

stop()->
	mnesia:stop().

abort(Reason)->
	mnesia:abort(Reason).

transaction(Fun)->
	mnesia:transaction(Fun).
transaction(Fun, Retries)->
	mnesia:transaction(Fun, Retries).
transaction(Fun, Args, Retries)->
	mnesia:transaction(Fun, Args, Retries).


ets(Fun)->
	mnesia:ets(Fun).
ets(Fun, Args)->
	mnesia:ets(Fun, Args).

activity(Kind, Fun)->
	mnesia:activity(Kind, Fun).
activity(Kind, Fun, Args)->
	mnesia:activity(Kind, Fun, Args).
activity(Kind, Fun, Args, Mod)->
	mnesia:activity(Kind, Fun, Args, Mod).

write(Record)->
	?MNESIA_OPERATER("Record:~p",[Record]),
	Res = mnesia:write(Record),
	?MNESIA_OPERATER_RESULT("(Record:~p) Result:~p",[Record,Res]),
	Res.
write(Tab, Val, LockKind)->
	?MNESIA_OPERATER("Tab:~p,Val:~p,LockKind:~p",[Tab,Val,LockKind]),
	Res = mnesia:write(Tab, Val, LockKind),
	?MNESIA_OPERATER_RESULT("(Tuple:~p,Val:~p,LockKind:~p) Result:~p",[Tab,Val,LockKind,Res]),
	Res.
write(Tid, Ts, Tab, Val, LockKind)->
	?MNESIA_OPERATER("Tid:~p,Ts:~p,Tab:~p,Val:~p,LockKind:~p",[Tid,Ts,Tab,Val,LockKind]),
	Res = mnesia:write(Tid, Ts, Tab, Val, LockKind),
	?MNESIA_OPERATER_RESULT("(Tid:~p,Ts:~p,Tab:~p,Val:~p,LockKind:~p) Result:~p",[Tid,Ts,Tab,Val,LockKind,Res]),
	Res.
	
delete(Tuple)->
	?MNESIA_OPERATER("Tuple:~p",[Tuple]),
	Res = mnesia:delete(Tuple),
	?MNESIA_OPERATER_RESULT("(Tuple:~p) Result:~p",[Tuple,Res]),
	Res.
delete(Tab, Key, LockKind)->
	?MNESIA_OPERATER("Tab:~p,Key:~p,LockKind:~p",[Tab,Key,LockKind]),
	Res = mnesia:delete(Tab, Key, LockKind),
	?MNESIA_OPERATER_RESULT("(Tab:~p,Key:~p,LockKind:~p) Result:~p",[Tab,Key,LockKind,Res]),
	Res.
delete(Tid, Ts, Tab, Key, LockKind)->
	?MNESIA_OPERATER("Tid:~p,Ts:~p,Tab:~p,Key:~p,LockKind:~p",[Tid,Ts,Tab,Key,LockKind]),
	Res = mnesia:delete(Tid, Ts, Tab, Key, LockKind),
	?MNESIA_OPERATER_RESULT("(Tid:~p,Ts:~p,Tab:~p,Key:~p,LockKind:~p) Result:~p",[Tid,Ts,Tab,Key,LockKind,Res]),
	Res.

delete_object(Tuple)->
	?MNESIA_OPERATER("Tuple:~p",[Tuple]),
	Res = mnesia:delete_object(Tuple),
	?MNESIA_OPERATER_RESULT("(Tuple:~p) Result:~p",[Tuple,Res]),
	Res.
delete_object(Tab, Val, LockKind)->
	?MNESIA_OPERATER("Tab:~p,Val:~p,LockKind:~p",[Tab,Val,LockKind]),
	Res = mnesia:delete_object(Tab, Val, LockKind),
	?MNESIA_OPERATER_RESULT("(Tab:~p,Val:~p,LockKind:~p) Result:~p",[Tab,Val,LockKind,Res]),
	Res.
delete_object(Tid, Ts, Tab, Val, LockKind)->
	?MNESIA_OPERATER("Tid:~p,Ts:~p,Tab:~p,Val:~p,LockKind:~p",[Tid,Ts,Tab,Val,LockKind]),
	Res = mnesia:delete_object(Tid, Ts, Tab, Val, LockKind),
	?MNESIA_OPERATER_RESULT("(Tid:~p,Ts:~p,Tab:~p,Val:~p,LockKind:~p) Result:~p",[Tid,Ts,Tab,Val,LockKind,Res]),
	Res.
	
read(Tuple)->
	Res = mnesia:read(Tuple).
read(Tab, Key)->
	Res = mnesia:read(Tab, Key).
read(Tab, Key, LockKind)->
	Res = mnesia:read(Tab, Key, LockKind).
read(Tid, Ts, Tab, Key, LockKind)->
	Res = mnesia:read(Tid, Ts, Tab, Key, LockKind).
	
match_object(Pattern)->
	mnesia:match_object(Pattern).
match_object(Tab, Pat, LockKind)->
	mnesia:match_object(Tab, Pat, LockKind).
match_object(Tid, Ts, Tab, Pat, LockKind)->
	mnesia:match_object(Tid, Ts, Tab, Pat, LockKind).
	
index_read(Tab, Key, Attr)->
	mnesia:index_read(Tab, Key, Attr).
index_read(Tid, Ts, Tab, Key, Attr, LockKind)->
	mnesia:index_read(Tid, Ts, Tab, Key, Attr, LockKind).
	
foldl(Fun, Acc0, Tab)->
	mnesia:foldl(Fun, Acc0, Tab).
foldl(Fun, Acc, Tab, LockKind)->
	mnesia:foldl(Fun, Acc, Tab, LockKind).
foldr(Fun, Acc, Tab)->
	mnesia:foldr(Fun, Acc, Tab).
foldr(Fun, Acc, Tab, LockKind)->
	mnesia:foldr(Fun, Acc, Tab, LockKind).
	
dirty_write(Record)->
	?MNESIA_OPERATER("Record:~p",[Record]),
	Res = mnesia:dirty_write(Record),
	?MNESIA_OPERATER_RESULT("(Record:~p) Result:~p",[Record,Res]),
	Res.
dirty_write(Tab, Record)->
	?MNESIA_OPERATER("Tab:~p,Record:~p",[Tab,Record]),
	Res = mnesia:dirty_write(Tab, Record),
	?MNESIA_OPERATER_RESULT("(Tab:~p,Record:~p) Result:~p",[Tab,Record,Res]),
	Res.
	
dirty_delete(TabKeyTuple)->
	?MNESIA_OPERATER("TabKeyTuple:~p",[TabKeyTuple]),
	Res = mnesia:dirty_delete(TabKeyTuple),
	?MNESIA_OPERATER_RESULT("(TabKeyTuple:~p) Result:~p",[TabKeyTuple,Res]),
	Res.
dirty_delete(Tab, Key)->
	?MNESIA_OPERATER("Tab:~p,Key:~p",[Tab,Key]),
	Res = mnesia:dirty_delete(Tab, Key),
	?MNESIA_OPERATER_RESULT("(Tab:~p,Key:~p) Result:~p",[Tab,Key,Res]),
	Res.
	
dirty_delete_object(Record)->
	?MNESIA_OPERATER("Record:~p",[Record]),
	Res = mnesia:dirty_delete_object(Record),
	?MNESIA_OPERATER_RESULT("(Record:~p) Result:~p",[Record,Res]),
	Res.
dirty_delete_object(Tab, Record)->
	?MNESIA_OPERATER("Tab:~p,Record:~p",[Tab,Record]),
	Res = mnesia:dirty_delete_object(Tab, Record),
	?MNESIA_OPERATER_RESULT("(Tab:~p,Record:~p) Result:~p",[Tab,Record,Res]),
	Res.
	
dirty_read(TabKeyTuple)->
	mnesia:dirty_read(TabKeyTuple).
dirty_read(Tab, Key)->
	mnesia:dirty_read(Tab, Key).
	
dirty_index_match_object(Pattern, Attr)->
	mnesia:dirty_index_match_object(Pattern, Attr).
dirty_index_match_object(Tab, Pat, Attr)->
	mnesia:dirty_index_match_object(Tab, Pat, Attr).
	
dirty_index_read(Tab, Key, Attr)->
	mnesia:dirty_index_read(Tab, Key, Attr).
	
table_info(Tab, Item)->
	mnesia:table_info(Tab, Item).
table_info(Tid, Ts, Tab, Item)->
	mnesia:table_info(Tid, Ts, Tab, Item).
	
system_info(Item)->
	?MNESIA_OPERATER("Item:~p",[Item]),
	Res = mnesia:system_info(Item),
	?MNESIA_OPERATER_RESULT("(Item:~p) Result:~p",[Item,Res]),
	Res.
	
create_schema(Ns)->
	mnesia:create_schema(Ns).
create_schema(Ns, Properties)->
	mnesia:create_schema(Ns, Properties).
	
delete_schema(Ns)->
	mnesia:delete_schema(Ns).
	
backup(Opaque)->
	mnesia:backup(Opaque).
backup(Opaque, Mod)->
	mnesia:backup(Opaque, Mod).
	
traverse_backup(Src, Dest, Fun, Acc)->
	mnesia:traverse_backup(Src, Dest, Fun, Acc).
traverse_backup(Src, SrcMod, Dest, DestMod, Fun, Acc)->
	mnesia:traverse_backup(Src, SrcMod, Dest, DestMod, Fun, Acc).
	
create_table(Arg)->
	?MNESIA_OPERATER("Arg:~p",[Arg]),
	mnesia:create_table(Arg).
create_table(Name, Arg)->
	?MNESIA_OPERATER("Name:~p,Arg:~p",[Name,Arg]),
	mnesia:create_table(Name, Arg).
	
delete_table(Tab)->
	mnesia:delete_table(Tab).
	
add_table_copy(Tab, Node, StorageType)->
	mnesia:add_table_copy(Tab, Node, StorageType).
del_table_copy(Tab, Node)->
	mnesia:del_table_copy(Tab, Node).
move_table_copy(Tab, From, To)->
	mnesia:move_table_copy(Tab, From, To).
	
change_table_copy_type(Tab, Node, StorageType)->
	mnesia:change_table_copy_type(Tab, Node, StorageType).
change_table_majority(Tab, MajorityFlag)->
	mnesia:change_table_majority(Tab, MajorityFlag).
	
clear_table(Tab)->
	mnesia:clear_table(Tab).
clear_table(Tid, Ts, Tab, Obj)->
	mnesia:clear_table(Tid, Ts, Tab, Obj).
	
wait_for_tables(Tabs, Timeout)->
	mnesia:wait_for_tables(Tabs, Timeout).
	
foldl(ActivityId, Opaque, Fun, Acc, Tab, LockKind)->
	mnesia:foldl(ActivityId, Opaque, Fun, Acc, Tab, LockKind).
foldr(ActivityId, Opaque, Fun, Acc, Tab, LockKind)->
	mnesia:foldr(ActivityId, Opaque, Fun, Acc, Tab, LockKind).
	
table(Tab)->
	mnesia:table(Tab).
table(Tab,Opts)->
	mnesia:table(Tab,Opts).
	
new(Name, Options)->
	?ETS_OPERATER_START("Name:~p, Options:~p",[Name,Options]),
	Returns = ets:new(Name, Options),
	?ETS_OPERATER_END("Results=~p",[Returns]),
	Returns.
