-module(version).
-include("login_pb.hrl").
-define(VERSION,"13.01.04.1644-DEBUG").
-export([make_version/0,version/0]).
make_version()->
	login_pb:encode_proto_msg(server_version_s2c,#server_version_s2c{v = ?VERSION}).
	

version()->
	?VERSION.