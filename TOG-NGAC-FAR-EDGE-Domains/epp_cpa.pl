% Context-dependent Policy Adaptation (CPA)

:- module(epp_cpa, [condition_context_variable_map/2, read_context_cache/2]).

:- use_module(dpl_conditions).
:- use_module(epp_era).


% ------------------------------------------------------------------------
% epp_cpa initialization - called by module ngac
%

init :-
    init_context_cache,
    init_context_change_notifications,
    true.


% ------------------------------------------------------------------------
% CONTEXT VARIABLES
%
%   the file context.pl is consulted by init_context_cache
%
%   it contains a definition for context_variables/1
%   the empty def below is the default in case the file is empty

:- dynamic context_variables/1.
:- multifile context_variables/1.

context_variables([]).

% ------------------------------------------------------------------------
% MAPPING OF CONDITION VARIABLES TO CONTEXT VARIABLES
%
% condition_context_variable_map(ConditionVar, ContextVar)

:- dynamic condition_context_variable_map/2.
:- multifile condition_context_variable_map/2.

% these declarations should occur in the file context.pl


% ------------------------------------------------------------------------
% CONTEXT VARIABLE CACHE
%    context variables obtained from the context system are cached for
%    mapping to condition variables used in DPL condition predicates
%
%    ctx_cache(ContextVarName, ContextVarType, ContextVarValue)
%    ContextVarName uniquely identifies entry.
%    read_context_cache/3 returns Type and Value
%    update_context_cache/3 Type must be var or match what is stored

:- dynamic ctx_cache/3, ctx_cache_initialized/1.

ctx_cache(last_update,number,0).

ctx_cache_initialized(no).

init_context_cache :-
    (   exists_file('context.pl')
    ->  consult(context)
    ;   true
    ),
    findall(CtxVar, (context_variables(Vars), member(CtxVar,Vars)), CtxVars),
    forall( ( member(CtxVar:Type,CtxVars), retrieve_context_variable(CtxVar,CtxVal) ),
            update_context_cache(CtxVar,Type,CtxVal)
          ),
    retractall(ctx_cache_initialized(_)),
    assert(ctx_cache_initialized(yes)),
    true.

update_context_cache(CtxVar,CtxVal) :- update_context_cache(CtxVar,_,CtxVal).
update_context_cache(CtxVar,Type,Value) :- atom(CtxVar), ground(Value),
    context_variables(CVs), memberchk(CtxVar:Type,CVs), !, % update is only for value not type
    retractall(ctx_cache(CtxVar,_,_)), % variable names must be unique
    assert(ctx_cache(CtxVar,Type,Value)),
    % update (time of) last_update entry
    get_time(TS),
    retractall(ctx_cache(last_update,UT,_)),
    assert(ctx_cache(last_update,UT,TS)).
update_context_cache(_,_,_). % succeed silently if CtxVar not defined

read_context_cache(CtxVar,CtxVal) :- atom(CtxVar), var(CtxVal), !,
    read_context_cache(CtxVar:_,CtxVal).
read_context_cache(CtxVar:Type,CtxVal) :- % read_context_cache with Type
    ctx_cache_initialized(yes), atom(CtxVar), var(CtxVal), !,
    ctx_cache(CtxVar,Type,CtxVal).
read_context_cache(_:_,undefined).


% ------------------------------------------------------------------------
% CPA-CME CONTEXT MONITORING AND EXTRACTION INTERFACE
%
%   context_change_notification is invoked by the Context System
%   retrieve_context_variables invokes the Context System

init_context_change_notifications :-
    true.

% CONTEXT CHANGE NOTIFICATION
%   invoked by the Context System
%   VarsVals is a list of Variable:Value pairs

context_change_notification :-
    context_variables(CVs),
    findall(Var:Val, (member(Var:_,CVs), retrieve_context_variable(Var,Val)), VarsVals),
    update_context_variables(VarsVals),
    report_event(context_change).

context_change_notification(VarsVals) :- is_list(VarsVals), !,
    update_context_variables(VarsVals),
    report_event(context_change).
context_change_notification(Var:Val) :- atom(Var), ground(Val), !,
    update_context_cache(Var,Val),
    report_event(context_change).

% update multiple variables in the context cache
%

update_context_variables([]) :- !.
update_context_variables([Var:Val|VarsVals]) :-
    update_context_cache(Var,Val),
    update_context_variables(VarsVals).

% actively query the context system for one or more context variables
% retrieve context variable values from the Context System

retrieve_context_variables([],[]).
retrieve_context_variables([Var|Vars],[Val|Vals]) :-
    retrieve_context_variable(Var,Val),
    retrieve_context_variables(Vars,Vals).

retrieve_context_variable(CtxVar,CtxVal) :-
    sim_context_var(CtxVar,CtxVal). % use context system simulation for now


% ------------------------------------------------------------------------
% CONTEXT MONITORING AND EXTRACTION SIMULATION
%   context_variable_change/2 generates a context_change_notification
%
%
sim_context_var(day_of_the_week, DayOfWeek) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,_Hour,_Min,_Sec,_,_,_),
    ShortDate = date(Year,Month,Date),
    day_of_the_week(ShortDate,Day),
    nth1(Day,['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'],DayOfWeek),
    true.
sim_context_var(adminLockdown,false) :- !.
% ...
sim_context_var(_,undefined).

% simulate change of a single context variable
context_variable_change(VarName:Value) :- atom(VarName), ground(Value), !,
    context_change_notification(VarName:Value).

% simulate change of a list of context variables
context_variable_change(VarsVals) :- is_list(VarsVals), !,
    context_change_notification(VarsVals).
