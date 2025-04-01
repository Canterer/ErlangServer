-module(login_package).

%%
%% Include files
%%
-include("login_pb.hrl").
-include("login_def.hrl").
-include("user_auth.hrl").

%%
%% Exported Functions
%%
-export([handle/3]).

%%
%% API Functions
%%

handle(#user_auth_c2s{username=UserName, userid=UserId, time=Time, cm=Adult, 
					  flag=AuthResult,userip = UserIp,type = Type,sid = SId,
					  serverid = ServerId,openid=OpenId,openkey=OpenKey,
					  appid=AppId,pf=Pf,pfkey=PfKey},FromProcName,_RolePid)->
	UserAuth =#user_auth{username=UserName, userid=UserId, lgtime=Time, 
						 cm=Adult, flag=AuthResult,userip=UserIp,type=Type,
						 sid=SId,openid=OpenId,openkey=OpenKey,appid=AppId,
						 pf=Pf,pfkey=PfKey},
	% base_tcp_client_statem:start_auth(node(),FromProcName,ServerId,UserAuth);
	base_tcp_client_statem:apply_component(auth_component,start_auth,[node(),FromProcName,ServerId,UserAuth]);
handle(#player_select_role_c2s{roleid=RoleId,lineid=LineId},FromProcName,_RolePid)->
	% base_tcp_client_statem:role_into_map_request(node(), FromProcName, RoleId,LineId);
	base_tcp_client_statem:apply_component(request_line_map_component,role_into_map_request,[node(), FromProcName, RoleId,LineId]);

handle(#role_line_query_c2s{mapid=MapId},FromProcName,_RolePid)->
	% base_tcp_client_statem:line_info_request(node(), FromProcName,MapId );
	base_tcp_client_statem:apply_component(request_line_map_component,line_info_request,[node(), FromProcName,MapId]);
 
handle(#create_role_request_c2s{role_name=RoleName,gender=Gender,classtype=ClassType},FromProcName,_RolePid)->
	% base_tcp_client_statem:role_create_request(node(), FromProcName,RoleName,Gender,ClassType);
	base_tcp_client_statem:apply_component(create_role_component,role_create_request,[node(), FromProcName,RoleName,Gender,ClassType]);

%% handle(#is_visitor_c2s{t=Time,f=Flag},FromProcName,_RolePid)->
%% 	base_tcp_client_statem:start_auth(node(), FromProcName,Time,Flag);

%% handle(#is_finish_visitor_c2s{t=Time,f=Flag,u=AccountName},FromProcName,_RolePid)->
%% 	auth_processor:auth(node(), FromProcName, Time, Flag, AccountName);

handle(#reset_random_rolename_c2s{},FromProcName,_RolePid)->
	base_tcp_client_statem:apply_component(create_role_component,reset_random_rolename,[node(), FromProcName]);


handle(_Msg,_Proc,_Context)->
	ok.
%%
%% Local Functions
%%

