%% Description: TODO: Add description to base_gen_id_util
-module(base_gen_id_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
	get_idmax/1,
	get_idmax/2,
	update_idmax/2
]).

%%
%% API Functions
%%
get_idmax(Type)->
	case base_db_dal_util:read_rpc(idmax,Type) of
		{ok,[R]}->{_,_,Counter} = R,Counter;
		_->0
	end.

get_idmax(Type,OrigId)->
	case base_db_dal_util:read_rpc(idmax,Type) of
		{ok,[R]}->{_,_,Counter} = R,Counter;
		_->OrigId
	end.

update_idmax(Type,NewValue)->
	base_db_dal_util:async_write_rpc({idmax,Type,NewValue}).


%%
%% Local Functions
%%

