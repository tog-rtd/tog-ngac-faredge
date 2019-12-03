% POLICY QUERY API
:- module(pqapi, []).

:- use_module(audit,[audit_gen/2]).
:- use_module(param).
:- use_module(dpl).
:- use_module(pdp).
:- use_module(sessions).
:- use_module(domains).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Policy Query API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(pqapi), root_apis(pqapi), []).
:- http_handler(root('pqapi/'), api_unimpl, [prefix]).
:- http_handler(root(pqapi/access), pqapi_access, [prefix]).
:- http_handler(root(pqapi/getobjinfo), pqapi_getobjinfo, [prefix]).
:- http_handler(root(pqapi/paramecho), pqapi_paramecho, [prefix]).

pqapi([access,getobjectinfo]). % POLICY QUERY API

% Global Policy Query API
%:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(gpqapi), root_apis(gpqapi), []).
:- http_handler(root('gpqapi/'), api_unimpl, [prefix]).
:- http_handler(root(gpqapi/gaccess), gpqapi_gaccess, [prefix]).
:- http_handler(root(gpqapi/ggetinfo), gpqapi_ggetinfo, [prefix]).

gpqapi([gaccess,ggetinfo]). % GLOBAL POLICY QUERY API


%
% Policy Query API
%
pqapi_access(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[user(User,[atom]),
				 ar(AR,[atom]),
				 object(Object,[atom])
				]),
	    E,writeln('missing parameter')),	!,
	(   nonvar(E)
	->  writeln(failure),
	    audit_gen(policy_query, access(failure))
	;   access_response(User,AR,Object)
	).

pqapi_getobjinfo(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[object(O,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   nonvar(E)
	->  writeln(failure),
	    audit_gen(policy_query, getobjinfo(failure))
	;   param:current_policy(P),
	    dpl:policy(P,Pr),
	    %format('policy=~a:~a~n',[P,Pr]), format('object=~a~n',O),
	    dpl:object(P:Pr,O), !,
	    dpl:object(P:Pr,O,Oclass,Inh,Host,Path,BaseType,BaseName),
	    audit_gen(pq_objinfo, getojfinfo(P,O)),
	    writeq(objectinfo(O,Oclass,Inh,Host,Path,BaseType,BaseName))
	    %format('object=~a,oclass=~a,inh=~a,host=~a,path=~a,basetype=~a,basename=~a~n',
	%	   [O,Oclass,Inh,Host,Path,BaseType,BaseName])
	).

pqapi_paramecho(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[],[form_data(Params)]),
	    E,writeln('missing parameter')),	!,
	(   nonvar(E)
	->  writeln(failure)
	;  format('~q~n',[Params])
	).

access_response(User,AR,Object) :- param:current_policy(deny), !,
	access_deny(deny,User,AR,Object).

access_response(User,AR,Object) :- param:current_policy(grant), !,
	access_grant(grant,User,AR,Object).

access_response(User,AR,Object) :-
	param:current_policy(Policy),
	(   Policy == none
	->  writeln('no current policy'), writeln('failure')
	;
	(   access_check(Policy,(User,AR,Object))
	->  access_grant(Policy,User,AR,Object)
	;   access_deny(Policy,User,AR,Object)
	)).

access_grant(Policy,UserOrSession,AR,Object) :-
	( sessions:is_session(UserOrSession,U), User = session(U) ; User = UserOrSession ),
	% audit record will show session(<user>) if session invocation, otherwise just <user>
	audit_gen(pq_grant, access(Policy,(User,AR,Object))),
	writeln(grant). % actual response to caller

access_deny(Policy,UserOrSession,AR,Object) :-
	( sessions:is_session(UserOrSession,U), User = session(U) ; User = UserOrSession ),
	% audit record will show session(<user>) if session invocation, otherwise just <user>
	audit_gen(pq_deny, access(Policy,(User,AR,Object))),
	writeln(deny). % actual response to caller

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

use_pqapi(_) :-
	std_resp_prefix,
	format('Use (g)pqapi as root for policy query APIs~n'),
	list_apis(pqapi), list_apis(gpqapi).

use_paapi(_) :-
	std_resp_prefix,
	format('Use (g)paapi as root for policy administration APIs~n'),
	list_apis(paapi), list_apis(gpaapi).

use_valid_api(_) :-
	format('Use (g)paapi for policy admin, (g)pqapi for policy query~n').

std_resp_prefix :- format('Content-type: text/plain~n~n').


%
% Global Policy Query API
%
%     gaccess(G1,CommOp,G2) may G1 perform comm operation CommOp to G2 ?
%
%     ggetinfo( )
%

gpqapi_gaccess(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[src(Src,[atom]),
				 op(Op,[atom]),
				 dst(Dst,[atom])
				]),
	    E,writeln('missing parameter')),	!,
	(   nonvar(E)
	->  true
	;   gaccess_response(Src,Op,Dst)
	).

gpqapi_ggetinfo(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[]),
	    E,writeln('missing parameter')),
	!,
	(   nonvar(E)
	->  true
	;  true
	).

% gaccess_response(User,AR,Object) :- server:server_options(Opt),
% memberchk(deny(true),Opt), !,
%       param:current_policy(Policy),
%       gaccess_deny(Policy,User,AR,Object).

% gaccess_response(User,AR,Object) :- server:server_options(Opt),
% memberchk(grant(true),Opt), !,
%       param:current_policy(Policy),
%       gaccess_grant(Policy,User,AR,Object).

gaccess_response(Src,Op,Dst) :-
	param:current_policy(LocalPolicy),
	(   LocalPolicy == none
	->  writeln('no current local policy'),
            gaccess_deny(LocalPolicy,Src,Op,Dst),
	    !, fail
	;   true
	),
	param:current_gpolicy(GlobalPolicy),
	(   GlobalPolicy == none
	->  writeln('no current global policy'),
            gaccess_deny(GlobalPolicy,Src,Op,Dst)
	;
	(   gaccess_check(LocalPolicy,GlobalPolicy,(Src,Op,Dst))
	->  gaccess_grant(LocalPolicy,Src,Op,Dst)
	;   gaccess_deny(LocalPolicy,Src,Op,Dst)
	)),
	!.
gaccess_response(_,_,_).

gaccess_grant(Policy,Src,Op,Dst) :-
	audit_gen(gpq_grant, gaccess(Policy,(Src,Op,Dst))),
	writeln(grant).

gaccess_deny(Policy,Src,Op,Dst) :-
	audit_gen(gpq_deny, gaccess(Policy,(Src,Op,Dst))),
	writeln(deny).



%
