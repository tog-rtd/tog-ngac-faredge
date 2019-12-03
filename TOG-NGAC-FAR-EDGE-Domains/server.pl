% Lightweight NGAC server

:- module(server, [server/1,server/2,server_with_args/1]).

:- use_module(audit,[audit_gen/2]).
:- use_module(param).
:- use_module(dpl).
:- use_module(sessions).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/http_wrapper)).
%:- use_module(library(http/http_header)).
%:- use_module(library(http/http_parameters)).

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

server(Port) :-
	create_server_audit_log,
	http_server(http_dispatch, [port(Port)]),
	format('ngac-server started~n'),
	audit_gen(ngac_start, success),
	param:server_sleeptime(S), my_sleep(S).

server(Port,Token) :-
	param:setparam(admin_token,Token),
	server(Port).

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
	create_server_audit_log,
	http_server(http_dispatch, [port(QPort)]),
	format('ngac-server started~n'),
	audit_gen(ngac_start, success),

	% run self-test here if turned on in param or command line
	% ngac:self_test_all,

	(   memberchk(importfile(Pfile),Opts) ; true ),
	(   var(Pfile)
	->  true % initial load file not specified
	;   (   ( atom(Pfile), exists_file(Pfile) )
	    ->	dpl:load_decl_policy(Pfile,PolicyName),
		pap:set_current_policy(PolicyName),
		format('policy ~q loaded from ~a~n',[PolicyName,Pfile]),
		audit_gen(policy_admin, importopt(Pfile,PolicyName,success))
	    ;   format('policy file load error: ~a~n',[Pfile]),
		audit_gen(policy_admin, importopt(Pfile,failure))
	    )
	),
	param:server_sleeptime(S), my_sleep(S),
	true.

my_sleep(S) :-
	sleep(S),
	periodic_goals,
	my_sleep(S).

periodic_goals :-
	% add periodic goals here
	true.

create_server_audit_log :- param:audit_logging(file), !,
	audit:gen_time_stamp(TS),
	atomic_list_concat(['LOG/audit_log','_',TS],LogFile),
	format('Audit log file: ~w~n',LogFile),
	open(LogFile,append,AudStream),
	param:setparam(audit_stream,AudStream).
create_server_audit_log.


