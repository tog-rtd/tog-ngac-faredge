% Lightweight NGAC server

:- module(server, [server/1,server/2,server_with_args/1]).

:- use_module(n_audit,[audit_gen/2]).
:- use_module(param).
:- use_module(spld).
:- use_module(sessions).


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

% SAFIRE server extended API
% Policy Administration API
:- http_handler(root(paapi), root_apis(paapi), []).
:- http_handler(root('paapi/'), api_unimpl, [prefix]).
:- http_handler(root(paapi/add), paapi_add, [prefix]).
:- http_handler(root(paapi/delete), paapi_delete, [prefix]).
:- http_handler(root(paapi/getpol), paapi_getpol, [prefix]).
:- http_handler(root(paapi/setpol), paapi_setpol, [prefix]).
:- http_handler(root(paapi/combinepol), paapi_combinepol, [prefix]).
:- http_handler(root(paapi/importpol), paapi_importpol, [prefix]).
:- http_handler(root(paapi/load), paapi_importpol, [prefix]).
:- http_handler(root(paapi/loadi), paapi_importpoli, [prefix]).
:- http_handler(root(paapi/readpol), paapi_readpol, [prefix]).
:- http_handler(root(paapi/purgepol), paapi_purgepol, [prefix]).
:- http_handler(root(paapi/unload), paapi_purgepol, [prefix]).
:- http_handler(root(paapi/initsession), paapi_initsession, [prefix]).
:- http_handler(root(paapi/endsession), paapi_endsession, [prefix]).

pqapi([access,getobjectinfo]). % POLICY QUERY
paapi([add,delete,getpol,setpol,combinepol,load,importpol,unload,purgepol,initsession,endsession]). % POLICY ADMIN

%
% ngac-server command line options
%
%    --deny    -d
%    --permit  --grant  -g
%    --port    --portnumber --pqport -p    <integer>
%    --import  --policy --load -i -l   <filename>
%    --selftest -s
%    --token   -t    <admintoken>
%    --verbose  -v
%
server_opt_spec([
        [opt(deny), type(boolean), default(false), shortflags([d]), longflags(['deny']),
         help( 'respond to all access requests with deny' )],
	[opt(grant), type(boolean), default(false), shortflags([g]), longflags(['permit','grant']),
         help( 'respond to all access requests with grant' )],
        [opt(pqportnumber), meta('QP'), type(integer), shortflags([p]), longflags(['port','portnumber','pqport']),
         help( 'server listens for policy queries on port QP' )],
 %       [opt(paportnumber), meta('AP'), type(integer), shortflags([a]), longflags(['admin','paport']),
 %        help( 'server listens for policy admin on port AP' )],
        [opt(importfile), meta('FILE'), type(atom), shortflags([i,l]), longflags(['import','policy','load']),
         help( 'import/load policy from FILE' )],
        [opt(selftest), type(boolean), default(false), shortflags([s]), longflags(['selftest']),
         help( 'run self tests on startup' )],
        [opt(token), meta('TOKEN'), type(atom), shortflags([t]), longflags(['token']),
         help( 'policy administration requests must cite TOKEN' )],
        [opt(verbose), type(boolean), default(false), shortflags([v]), longflags(['verbose']),
         help( 'show all messages' )]
]).

:- dynamic server_options/1.
server_options([]).

%option_deny :- server_options(O), memberchk(deny(true),O).
%option_permit :- server_options(O), memberchk(grant(true),O).

server(Port) :-
	http_server(http_dispatch, [port(Port)]),
	format('ngac-server started~n'),
	param:server_sleeptime(S), my_sleep(S).

server(Port,Token) :-
	param:setparam(admin_token,Token),
	http_server(http_dispatch, [port(Port)]),
	format('ngac-server started~n'),
	param:server_sleeptime(S), my_sleep(S).

server_with_args(Argv) :-
	% format('Server Argv: ~q~n',[Argv]),
	% process the arguments
	server_opt_spec(OptSpec),
	catch(
	    opt_parse(OptSpec,Argv,Opts,_Positionals),
	    E, writeln('error in command line arguments')),
	!,
	(   nonvar(E)
	->  halt(1)
	;   retractall(server_options(_)), assert(server_options(Opts)),
	    % format('Opts: ~q~nPositionals: ~q~n',[Opts,Positionals]),
	    memberchk(pqportnumber(QPort),Opts)
	% , memberchk(pqportnumber(APort),Opts)
	),

	(   var(QPort)
	->  param:pqapi_port(QPort)
	;   true
	),

%	(   var(APort)
%	->  param:pqapi_port(APort)
%	;   true
%	),

	(   memberchk(grant(true),Opts)
	->  param:setparam(current_policy,grant)
	;   true
	),

	(   memberchk(deny(true),Opts)
	->  (   memberchk(grant(true),Opts)
	    ->  writeln('grant and deny options cannot both be true--exiting'), halt(1)
	    ;   param:setparam(current_policy,deny)
	    )
	;   true
	),

	(   memberchk(verbose(true),Opts)
	->  param:setparam(verbose,on)
	;   param:setparam(verbose,off)
	),

	(   memberchk(selftest(true),Opts) % currently ignored
	->  param:setparam(self_test,on)
	;   param:setparam(self_test,off)
	),

	(   ( memberchk(token(Token),Opts), atom(Token) )
	->  param:setparam(admin_token,Token)
	;   true
	),

	format('ngac-server starting on port ~d~n',[QPort]),
	% http_server_hook([port(QPort)]),
	http_server(http_dispatch, [port(QPort)]),
	format('ngac-server started~n'),

	% run self-test here if turned on in param or command line
	% ngac:self_test_all,

	(   memberchk(importfile(Pfile),Opts) ; true ),
	(   var(Pfile)
	->  true % initial load file not specified
	;   (   ( atom(Pfile), exists_file(Pfile) )
	    ->	spld:load_decl_policy(Pfile,PolicyName),
		spld:set_current_policy(PolicyName),
		format('policy ~q loaded from ~a~n',[PolicyName,Pfile])
	    ;   format('policy file load error: ~a~n',[Pfile])
	    )
	),
	param:server_sleeptime(S), my_sleep(S),
	true.

        my_sleep(S) :-
	        sleep(S),
                my_sleep(S).

%
% Policy Query API
%
pqapi_access(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[user(User,[atom]),				 ar(AR,[atom]),
				 object(Object,[atom])
				]),
	    E,writeln('missing parameter')),	!,
	(   nonvar(E)
	->  true
	;   access_response(User,AR,Object)
	).

pqapi_getobjinfo(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[object(O,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   nonvar(E)
	->  true
	;   param:current_policy(P),
	    spld:policy(P,Pr),
	    %format('policy=~a:~a~n',[P,Pr]), format('object=~a~n',O),
	    spld:object(P:Pr,O), !,
	    spld:object(P:Pr,O,Oclass,Inh,Host,Path,BaseType,BaseName),
	    writeq(objectinfo(O,Oclass,Inh,Host,Path,BaseType,BaseName))
	    %format('object=~a,oclass=~a,inh=~a,host=~a,path=~a,basetype=~a,basename=~a~n',
	%	   [O,Oclass,Inh,Host,Path,BaseType,BaseName])
	).

access_response(User,AR,Object) :- param:current_policy(deny), !,
	access_deny(deny,User,AR,Object).

access_response(User,AR,Object) :- param:current_policy(grant), !,
	access_grant(grant,User,AR,Object).

access_response(User,AR,Object) :-
	param:current_policy(Policy),
	(   Policy == none
	->  writeln('no current policy')
	;
	(   spld:access_check(Policy,(User,AR,Object))
	->  access_grant(Policy,User,AR,Object)
	;   access_deny(Policy,User,AR,Object)
	)).

access_grant(Policy,UserOrSession,AR,Object) :-
	( sessions:is_session(UserOrSession,U), User = session(U) ; User = UserOrSession ),
	% audit record will show session(<user>) if session invocation, otherwise just <user>
	audit_gen(access_grant, access(Policy,(User,AR,Object))),
	writeln(grant). % actual response to caller

access_deny(Policy,UserOrSession,AR,Object) :-
	( sessions:is_session(UserOrSession,U), User = session(U) ; User = UserOrSession ),
	% audit record will show session(<user>) if session invocation, otherwise just <user>
	audit_gen(access_deny, access(Policy,(User,AR,Object))),
	writeln(deny). % actual response to caller

%
% Policy Administration API
%

permitted_add_delete_policy_elements([user,object,assign]).

parse_add_delete_arguments(Request, Policy, PElement, Token) :-
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				     policy_element(PE,[atom]),
				     token(Token,[atom])
				   ]),
	    E,writeln('error parsing request parameters')),
	!,
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

paapi_add(Request) :-
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token), !,
	(   ( authenticate_token(Token), add_policy_element(Policy,PElement) )
	->  writeln(success)
	;   writeln('error adding element'), writeln(failure)
	).
paapi_add(_).

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
	->  writeln(failure)
	;
	    (   spld:compose_policies(P1,P2,Pc)
	    ->  writeln(success)
	    ;   writeln('error combining policies'), writeln(failure)
	    )
	).

paapi_delete(Request) :-
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token), !,
	(   ( authenticate_token(Token), delete_policy_element(Policy,PElement) )
	->  writeln(success)
	;   writeln('error deleting element'), writeln(failure)
	).
paapi_delete(_).

add_policy_element(P,user(U)) :- policy(P,PC), \+element(P:PC,user(U)),	!, passert( element(P:PC,user(U)) ).
add_policy_element(P,object(O)) :- policy(P,PC), \+element(P:PC,object(O)), !, passert( element(P:PC,object(O)) ).
add_policy_element(P,assign(E,Attr)) :-	policy(P,PC),
	( ( element(P:PC,user(E)), element(P:PC,user_attribute(Attr)) ) % must be user to user_attribute
	;
	  ( element(P:PC,object(E)), element(P:PC,object_attribute(Attr)) ) % or object to object_attribute
	),
	\+assign(P:PC,E,Attr), % must be no current assignment
	!,
	passert( assign(P:PC,E,Attr) ).

delete_policy_element(P,user(U)) :- policy(P,PC), element(P:PC,user(U)),
	\+assign(P:PC,U,_), % must be no current assignment of the user
	!,
	pretract( element(P:PC,user(U)) ).
delete_policy_element(P,object(O)) :- policy(P,PC), element(P:PC,object(O)),
	\+assign(P:PC,O,_), % must be no current assignment of the object
	!,
	pretract( element(P:PC,object(O)) ).
delete_policy_element(P,assign(E,Attr)) :-  policy(P,PC), assign(P:PC,E,Attr), !, pretract( assign(P:PC,E,Attr) ).

passert(PI) :-	%format('asserting ~q~n',[PI]),
	assert(spld:PI).
pretract(PI) :- %format('retracting ~q~n',[PI]),
	retractall(spld:PI).

paapi_getpol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;   param:current_policy(P),
	    writeln(P),
	    writeln(success)
	).

paapi_setpol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;
	(   ( spld:policy(P,_); P==all; P==allnc; P==grant; P==deny; P==none )
	->  spld:set_current_policy(P),
	    writeln(success)
	;   writeln('unknown policy'),
	    writeln('failure')
	)).

paapi_importpol(Request) :- % load policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyfile(Pfile,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;
	(   exists_file(Pfile)
	->  spld:load_decl_policy(Pfile,PolicyName),
	    format('policy ~q loaded from ~a~n',[PolicyName,Pfile]),
	    writeln(success)
	;   writeln('file error'),
	    writeln('failure')
	)).

paapi_importpoli(Request) :- % load policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyspec(Pspec,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;
	(   ground(Pspec)
	->  spld:load_decl_policy_immediate(Pspec,PolicyName),
	    format('policy ~q loaded immediate~n',[PolicyName]),
	    % pio:display_policy(PolicyName), % temporary for testing
	    writeln(success)
	;   writeln('malformed policy'),
	    writeln('failure')
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

paapi_purgepol(Request) :- % unload policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;
	(   spld:policy(P,_)
	->  spld:purge_policy(P),
	    (	param:current_policy(P)
	    ->	retract(param:current_policy(_)),
		assert(param:current_policy(none))
	    ;	true
	    ),
	    writeln(success)
	;   writeln('unknown policy'),
	    writeln('failure')
	)).

paapi_initsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),
				    user(U,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;
	(   \+is_session(S,_)
	->  initsession(S,U),
	    writeln(success)
	;   writeln('session already registered'),
	    writeln('failure')
	)).

paapi_endsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_token(Token) )
	->  writeln(failure)
	;
	(   is_session(S,_)
	->  endsession(S),
	    writeln(success)
	;   writeln('session unknown'),
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

use_pqapi(_) :-
	std_resp_prefix,
	format('Use pqapi as root for lightweight server policy query APIs~n'),
	list_apis(pqapi).

use_paapi(_) :-
	std_resp_prefix,
	format('Use paapi as root for lightweight server policy administration APIs~n'),
	list_apis(paapi).

use_valid_api(_) :- use_pqapi(_), use_paapi(_).

std_resp_prefix :- format('Content-type: text/plain~n~n').

authenticate_token(Token) :-
	param:admin_token(Token), !.
authenticate_token(_) :- writeln('authentication error'), fail.


