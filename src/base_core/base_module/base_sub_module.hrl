%%% Description : base sub module
-module(base_sub_module_manager).
-include("base_define_shared.hrl").

-export([
	start/3,
	start/4,
	start_link/3,
	start_link/4,
	call/2,
	call/3,
	cast/2
]).

defaultReturn(func)->
	func().

continue(func)->
	func().

branch(func)->
	func().


publicFuncList()->
	funcList;
AddPublicFuncList(Mod,Func)->
	funcList.append({Func,Mod}).
RemovePublicFuncList(Mod,Func)->
	funcList.remove({Func,Mod}).
publicFunc(FuncName, Args)->
	{Func,Mod} = funcList.get(FuncName),
	Mod:Func(Args).

updateValue(ValueName, NewValue)->
	NewValue.

AddBranchIn()
AddBranchOut()

AddLimitRun()

action().
	ok.

next().
	stateName,stateContent,actions.

initModule().
	ok.

tryRunModule:state().

limitRunModule().




