%% Description: TODO: Add description to auth component
-module(autoname_component).
-export([
	handle_event/4,
	reset_random_rolename/2,
	init_autoname/1,
	create_role/2
]).

-include("base_component_shared.hrl").

reset_random_rolename(_GateNode,GateProc)->
	GatePid = GateProc,    %% proc name is the remote pid
	?base_gen_statem:cast(GatePid, {reset_random_rolename}).

init_autoname(RoleList)->
	?ZSS(),
	%%auto_name
	case RoleList of
		[]->
			case autoname_op:init_autoname_s2c() of
				{Gname,Bname}->
					put(autoname,{Gname,Bname}),
					Message = login_pb:encode_proto_msg(init_random_rolename_s2c,#init_random_rolename_s2c{bn=Bname,gn=Gname}),
					base_tcp_client_statem:send_data(self(), Message);
				_->
					nothing
			end;
		_->
			nothing
	end.

create_role(RoleName,RoleId)->
	autoname_op:create_role(RoleName,RoleId).



handle_event(cast,{reset_random_rolename},rolelisting,StateData)->
% handle_rolelisting_state(cast,{reset_random_rolename}, StateData)->
	%%auto_name
	put(autoname,[]),
	case autoname_op:init_autoname_s2c() of
		{Gname,Bname}->
			Message = login_pb:encode_proto_msg(init_random_rolename_s2c,#init_random_rolename_s2c{bn=Bname,gn=Gname}),
			base_tcp_client_statem:send_data(self(), Message);
		_->
			nothing
	end,
	{next_state, rolelisting, StateData};
handle_event(Event, EventContent, StateName, StateData) ->
	unhandle.