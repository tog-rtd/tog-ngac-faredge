% Sessions
%   an ephemeral alternative to user ID in access queries

:- module(sessions, [is_session/2, initsession/2, endsession/1]).

:- dynamic session/2.

% session(SessionId, UserId)
session(0,0).

is_session(SessionId, UserId) :-
    (   ground(SessionId) ; ground(UserId) ), !,
    session(SessionId, UserId).

initsession(SessionId, UserId) :-
    ground(SessionId), ground(UserId), !,
    \+is_session(SessionId,UserId),
    assertz( session(SessionId,UserId) ).

endsession(SessionId) :-
    ground(SessionId), is_session(SessionId,_), !,
    retractall( session(SessionId,_) ).

