%%% Description : base sub module util
-module(base_sub_module_util).
-include("base_define_shared.hrl").

-export([
	ApplySubModule/3,
	ApplySubModule/4
]).

GetModuleData()->
	ok.
UpdateModuleData()->
	ok.

ApplySubModule(Mod,Func,Args)->
	erlang:apply(Mod,Func,Args).

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




