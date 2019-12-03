% ------------------------------------------------------------------------
% CONDITION VARIABLE DECLARATIONS
%   condition_variable(VariableName : VariableType)
%   Type is one of: list, boolean, number, name

condition_variable(condVar1:number).
condition_variable(condVar2:name).
condition_variable(condVar3:boolean).
condition_variable(lockdown:boolean).
condition_variable(today:name).

% ------------------------------------------------------------------------
% CONDITION PREDICATE DECLARATIONS & DEFINITIONS
%   condition_predicate(PredicateName,PredicateArgs)
%   PredicateArgs is a list of Types
%   Each Type is one of: list, boolean, number, name, any
%

condition_predicate(current_day_is_one_of, [list]).
condition_predicate(not_lockdown, []).

% condition predicate definitions

current_day_is_one_of(SetOfDays) :-
    condition_variable_value(local_day,Today),
    memberchk(Today,SetOfDays).

not_lockdown :-
    condition_variable_value(lockdown,false).
