%% Description: TODO: Add description to base_db_dmp_op
-module(base_db_dmp_op).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).
%%
%% API Functions
%%



%%
%% Local Functions
%%

async_write(BundleId,Object)->
	base_db_dmp_util:write(BundleId,Object).

async_write(BundleId,Table,TableKey,FieldIndex,FieldValue)->
	base_db_dmp_util:write(BundleId,Table,TableKey,FieldIndex,FieldValue).

async_write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldReplaceTuple)->
	base_db_dmp_util:write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldReplaceTuple).

async_delete(BundleId,Table,TableKey)->
	base_db_dmp_util:delete(BundleId,Table,TableKey).

flush_bundle(BundleId)->
	base_db_dmp_util:flush_bundle(BundleId).

flush_all()->
	base_db_dmp_util:flush_all().

sync_write(BundleId,Object)->
	base_db_dmp_util:sync_write(BundleId, Object).

sync_write(BundleId,Table,TableKey,FieldIndex,FieldValue)->
	base_db_dmp_util:sync_write(BundleId,Table,TableKey,FieldIndex,FieldValue).

sync_write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldReplaceTuple)->
	base_db_dmp_util:sync_write(BundleId,Table,TableKey,FieldIndex,FieldKey,FieldReplaceTuple).

sync_delete(BundleId,Table,TableKey)->
	base_db_dmp_util:sync_delete(BundleId,Table,TableKey).

