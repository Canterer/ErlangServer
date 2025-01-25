-include("common_define.hrl").
% -include("activity_define.hrl").
-module(base_timer_util).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_PER_DAY, 86400).
-export([
	send_after/2,
	send_after/3,
	cancel_timer/1
]).	 
		 	
send_after(Time,Message)->
	try
		TimeRef = erlang:send_after(erlang:trunc(Time),self(),Message),
		{ok,TimeRef}
	catch
		_:R-> {error,R}
	end.
	
send_after(Time,Pid,Message)->
	try
		TimeRef = erlang:send_after(erlang:trunc(Time),Pid,Message),
		{ok,TimeRef}
	catch
		_:R-> {error,R}
	end.
	
cancel_timer(TimeRef)->
	erlang:cancel_timer(TimeRef).





























  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  