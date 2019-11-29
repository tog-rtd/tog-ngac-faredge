% Next Generation Access Control audit manager
:- module(n_audit, [audit_gen/2, audit_set/1, audit_select/1, audit_deselect/1]).

% ngac audit operations
%   audit_gen generates audit record info for ngac audit
%     it leaves time stamp generation to the system audit function
%   audit_set unconditionally sets the parameter auditable_events
%     to the given list, except it will not permit the audit event
%     ngac_error to not be included.
%   audit_select addes the given list of ngac audit events to the
%     audit_selection
%   audit_deselect removes the given list of ngac audit events from the
%     current audit_selection

audit_gen(Event, Data) :-
    param:audit_selection(AS),
    (   memberchk(Event, AS)
    ->  sys_audit(ngac, Event, Data)
    ;   true
    ).

audit_set(Events) :-
    ground(Events), is_set(Events),
    param:auditable_events(Auditable),
    subset(Events,Auditable),
    union(Events,[ngac_error],SetEvents), % ngac_error cannot be removed from ngac audit_selection
    param:setparam(audit_selection,SetEvents), !.
audit_set(_) :-
    sys_audit(ngac, ngac_error, 'Failure to set audit_selection.').

audit_select(Event) :- atom(Event), !, audit_select([Event]).
audit_select(Events) :-
    ground(Events), is_set(Events),
    param:auditable_events(Auditable),
    subset(Events,Auditable),
    param:audit_selection(CurrentSelection),
    union(Events,CurrentSelection,SetEvents),
    param:setparam(audit_selection,SetEvents), !.
audit_select(_) :-
    sys_audit(ngac, ngac_error, 'Failure to add to audit_selection.').

audit_deselect(Event) :- atom(Event), !, audit_deselect([Event]).
audit_deselect(Events) :-
    ground(Events), is_set(Events),
    delete(Events,ngac_error,DeSelection), % cannot deselect ngac_error
    param:auditable_events(Auditable),
    subset(DeSelection,Auditable),
    param:audit_selection(CurrentSelection),
    subtract(CurrentSelection,DeSelection,NewSelection),
    param:setparam(audit_selection,NewSelection), !.
audit_deselect(_) :-
    sys_audit(ngac, ngac_error, 'Failure to delete from audit_selection.').

%
% Customise below for local system audit
%   always succeeds
%

sys_audit(Source, Event, Data) :- atom(Source), atom(Event), ground(Data),

    % Call system audit with args Source, Event, Data
    % Source may be derived from trusted channel by system audit
    % for PHANTOM use the PHANTOM Monitoring Framework API to create an audit log record
    % for provided audit system use audit generation APIs
    % for general use write to standard error stream
    % the server is a MF logger at the address AuditServ at port AuditPort and path AuditAPIpath
    % the address info is only representative

    % format(user_error, 'audit: ~q, ~q, ~q~n', [Source,Event,Data]), % for generic, comment or remove for real audit

    AuditServ='http://141.58.0.8',
    AuditPort=8010,
    AuditAPIpath=abc,
    atomic_list_concat([AuditServ,':',AuditPort,'/',AuditAPIpath,'/'],AuditLocn),
    format(atom(Query),'?source=~a&event=~a&data=~w',[Source,Event,Data]),
    atom_concat(AuditLocn,Query,_Auditq),
    % http_get(Auditq,Auditresult,[]), % log the audit record
    !.
sys_audit(_,_,_).
