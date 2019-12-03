context_variables([ % variables obtained from the context system
    day_of_the_week:name,
    contextVar1:number,
    contextVar2:name,
    contextVar3:boolean,
    adminLockdown:boolean
]).

condition_context_variable_map(today, day_of_the_week).
condition_context_variable_map(condVar1, contextVar1).
condition_context_variable_map(condVar2, contextVar2).
condition_context_variable_map(condVar3, contextVar3).
condition_context_variable_map(lockdown, adminLockdown).
