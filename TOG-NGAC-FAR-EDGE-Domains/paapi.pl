% POLICY ADMINISTRATION API
:- module(paapi, []).

:- use_module(audit,[audit_gen/2]).
:- use_module(param).
:- use_module(dpl).
:- use_module(sessions).
:- use_module(pap).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Policy Administration Web API
:- http_handler(root(paapi), root_apis(paapi), []).
:- http_handler(root('paapi/'), api_unimpl, [prefix]).
:- http_handler(root(paapi/add), paapi_add, [prefix]).
:- http_handler(root(paapi/addm), paapi_addm, [prefix]).
:- http_handler(root(paapi/delete), paapi_delete, [prefix]).
:- http_handler(root(paapi/deletem), paapi_deletem, [prefix]).
:- http_handler(root(paapi/getpol), paapi_getpol, [prefix]).
:- http_handler(root(paapi/setpol), paapi_setpol, [prefix]).
:- http_handler(root(paapi/combinepol), paapi_combinepol, [prefix]).
%:- http_handler(root(paapi/importpol), paapi_loadpol, [prefix]).
:- http_handler(root(paapi/load), paapi_loadpol, [prefix]).
:- http_handler(root(paapi/loadi), paapi_loadpoli, [prefix]).
:- http_handler(root(paapi/readpol), paapi_readpol, [prefix]).
:- http_handler(root(paapi/purgepol), paapi_unloadpol, [prefix]).
:- http_handler(root(paapi/unload), paapi_unloadpol, [prefix]).
:- http_handler(root(paapi/initsession), paapi_initsession, [prefix]).
:- http_handler(root(paapi/endsession), paapi_endsession, [prefix]).

% Global Policy Admin API
:- http_handler(root(gpaapi), root_apis(gpaapi), []).
:- http_handler(root('gpaapi/'), api_unimpl, [prefix]).
:- http_handler(root(gpaapi/getgpol), gpaapi_getgpol, [prefix]).
:- http_handler(root(gpaapi/setgpol), gpaapi_setgpol, [prefix]).

% POLICY ADMIN APIs
paapi([add,delete,getpol,setpol,combinepol,load,loadi,importpol,unload,purgepol,initsession,endsession]).

% GLOBAL POLICY ADMIN APIs
gpaapi([getgpol,setgpol]).

%
% Policy Administration API
%

paapi_add(Request) :-
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token), !,
	(   ( authenticate_token(Token), add_policy_element(Policy,PElement) )
	->  writeln(success),
	    audit_gen(policy_admin, add(Policy, PElement, success))
	;   writeln('error adding element'), writeln(failure),
	    audit_gen(policy_admin, add(Policy, PElement, failure))
	).
paapi_add(_) :-
	audit_gen(policy_admin, add(failure)).

paapi_delete(Request) :-
	audit_gen(policy_admin, delete(Request)),
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token), !,
	(   ( authenticate_token(Token), delete_policy_element(Policy,PElement) )
	->  writeln(success),
	    audit_gen(policy_admin, delete(Policy, PElement, success))
	;   writeln('error deleting element'), writeln(failure),
	    audit_gen(policy_admin, delete(Policy, PElement, failure))
	).
paapi_delete(_) :-
	audit_gen(policy_admin, delete(failure)).

parse_add_delete_arguments(Request, Policy, PElement, Token) :-
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				     % accept either of the following for backward compat
				     policy_element(P_E,[atom,optional(true)]),
				     policyelement(PE,[atom,optional(true)]),
				     token(Token,[atom])
				   ]),
	    E,writeln('error parsing request parameters')),
	!,
	(   ( ( var(P_E), var(PE) ) ; ( nonvar(P_E), nonvar(PE) ) ) % not neither or both
	->  writeln('error parsing request parameters'), writeln(failure), !, fail
	;   P_E = PE
	),
	(   nonvar(E)
	->  writeln(failure), !, fail
	;
	    (
		read_term_from_atom(PE,PElement,[]),
		% format('parsing arguments: ~a, ~q~n',[PE,PElement]),
		ground(PElement), PElement =.. [PEf|_PEargs],
		permitted_add_delete_policy_elements(Permitted),
		memberchk(PEf,Permitted),
		% format('add/delete args: policy=~a, element=~q~n',[Policy,PElement]),
		true
	    )
	;
	    writeln('error in argument'),
	    writeln(failure), !, fail
	).

paapi_addm(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				     policy_elements(EltList,[list(atom)]),
				     token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	% may be better to do separate parameter and authentication audit errors
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, addm(failure))
	;
	(   ( authenticate_token(Token), add_policy_elements(Policy,EltList) )
	->  writeln(success),
	    audit_gen(policy_admin, addm(Policy, 'Multiple elements', success))
	;   writeln('error adding elements'), writeln(failure),
	    audit_gen(policy_admin, addm(Policy, 'Multiple elements', failure))
	)).
paapi_addm(_) :-
	audit_gen(policy_admin, addm(failure)).

paapi_deletem(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				policy_elements(EltList,[list(atom)]),
				token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, deletem(failure))
	;
	(   ( authenticate_token(Token), delete_policy_elements(Policy,EltList) )
	->  writeln(success),
	    audit_gen(policy_admin, deletem(Policy, 'Multiple elements', success))
	;   writeln('error deleting elements'), writeln(failure),
	    audit_gen(policy_admin, deletem(Policy, 'Multiple elements', failure))
	)).
paapi_deletem(_) :-
	audit_gen(policy_admin, deletem(failure)).

paapi_getpol(Request) :- % get current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, getpol(failure))
	;   get_current_policy(P),
	    writeln(P),
	    writeln(success),
	    audit_gen(policy_admin, getpol(success))
	).

paapi_setpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, setpol(failure))
	;
	(   ( dpl:policy(P,_); P==all; P==allnc ; P==grant; P==deny, P==none )
	->  set_current_policy(P),
	    writeln(success),
	    audit_gen(policy_admin, setpol(P,success))
	;   writeln('unknown policy'),
	    writeln('failure'),
	    audit_gen(policy_admin, setpol(P,failure))
	)).

paapi_loadpol(Request) :- % load policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyfile(Pfile,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, load(failure))
	;
	(   ( exists_file(Pfile), load_policy(Pfile,PolicyName) )
	->
	    % TODO add check for: all, none, grant, deny
	    format('policy ~q loaded from ~a~n',[PolicyName,Pfile]),
	    writeln(success),
	    audit_gen(policy_admin, load(Pfile,PolicyName,success))
	;   writeln('file or load error'),
	    writeln('failure'),
	    audit_gen(policy_admin, load(Pfile,failure))
	)).

paapi_loadpoli(Request) :- % load policy immediate
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyspec(Pspec,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, loadi(failure))
	;
	(   ( ground(Pspec), load_policy_immediate(Pspec,PolicyName) )
	->
	    % TODO add check for: all, none, grant, deny
	    format('policy ~q loaded immediate~n',[PolicyName]),
	    % pio:display_policy(PolicyName), % temporary for testing
	    writeln(success),
	    audit_gen(policy_admin, loadi(Pspec,PolicyName,success))
	;   writeln('malformed policy or load error'),
	    writeln('failure'),
	    audit_gen(policy_admin, loadi(Pspec,failure))
	)).

paapi_unloadpol(Request) :- % unload policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, purgepol(failure))
	;
	(   dpl:policy(P,_)
	->  unload_policy(P),
	    writeln(success),
	    audit_gen(policy_admin, purgepol(P,success))
	;   writeln('unknown policy'),
	    writeln('failure'),
	    audit_gen(policy_admin, purgepol(P,failure))
	)).

paapi_combinepol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy1(P1,[atom]),
				     policy2(P2,[atom]),
				     combined(Pc,[atom]),
				     token(Token,[atom])
				    ]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, combinepol(failure))
	;
	    (   pap:compose_policies(P1,P2,Pc)
	    ->  writeln(success),
	        audit_gen(policy_admin, combinepol(P1,P2,Pc,success))
	    ;   writeln('error combining policies'), writeln(failure),
		audit_gen(policy_admin, combinepol(P1,P2,Pc,failure))
	    )
	).

paapi_initsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),
				    user(U,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, initsession(failure))
	;
	(   \+is_session(S,_)
	->  initsession(S,U),
	    writeln(success),
	    audit_gen(policy_admin, initsession(S,success))
	;   writeln('session already registered'),
	    writeln('failure'),
	    audit_gen(policy_admin, initsession(S,failure))
	)).

paapi_endsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, endsession(failure))
	;
	(   is_session(S,_)
	->  endsession(S),
	    writeln(success),
	    audit_gen(policy_admin, endsession(S,success))
	;   writeln('session unknown'),
	    writeln('failure'),
	    audit_gen(policy_admin, endsession(S,failure))
	)).


%
% GLOBAL POLICY ADMIN
%

gpaapi_getgpol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, getgpol(failure))
	;   get_current_gpolicy(GP),
	    writeln(GP),
	    writeln(success),
	    audit_gen(policy_admin, getgpol(success))
	).

gpaapi_setgpol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(GP,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure),
	    audit_gen(policy_admin, setgpol(failure))
	;
	(   dpl:policy(GP,_)
	->  set_current_gpolicy(GP),
	    writeln(success),
	    audit_gen(policy_admin, setgpol(GP,success))
	;   writeln('unknown policy'),
	    writeln('failure'),
	    audit_gen(policy_admin, setgpol(GP,failure))
	)).

paapi_readpol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom,default(current_policy)]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;
	(   ( P==current_policy, param:current_policy(Pa), Pa\==none ; policy(P,_), Pa=P )
	->  pio:display_policy(Pa),
	    writeln(success)
	;   writeln('unknown policy'),
	    writeln('failure')
	)).


%
%
%

api_unimpl(_) :-
	std_resp_prefix,
	format('Unimplemented API~n').

root_apis(Kind,_) :- std_resp_prefix, list_apis(Kind), !.
root_apis(_,_).

list_apis(Kind) :-
	format('Valid ~a paths:~n',[Kind]),
	G=..[Kind,APIs], call(G),
	foreach( member(A,APIs), writeln(A)).

%use_valid_api(_) :-
%	format('Use (g)paapi for policy admin, (g)pqapi for policy
%	query~n').

std_resp_prefix :- format('Content-type: text/plain~n~n').

authenticate_token(Token) :-
	param:admin_token(Token), !.
authenticate_token(_) :- writeln('authentication error'), fail.


