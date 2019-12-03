% Event Processing Point
%   http server startup

:- module(epp, [epp/1,epp/2,epp_with_args/1,load_erp/2,unload_erp/1,authenticate_epp_token/1,epp_log_gen/2]).

:- use_module(param).
:- use_module(erl).
:- use_module(epp_era).
:- use_module(epp_cpa).
:- use_module(epp_pcc).

:- use_module(audit,[audit_gen/2]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% ------------------------------------------------------------------------
% epp server - called by module ngac
%
%
% epp command line options
%
%    --port           -p <integer>
%    --erp --load     -e <filename>
%    --context        -x <filename>
%    --conditions     -c <filename>
%    --selftest       -s
%    --token          -t <admintoken>
%    --verbose        -v
%
epp_opt_spec([
        [opt(eppportnumber), meta('QP'), type(integer), shortflags([p]), longflags(['port']),
         help( 'server listens for policy queries on port QP' )],
        [opt(erpfile), meta('FILE'), type(atom), shortflags([l,e]), longflags(['load','erp']),
         help( 'load event-response package from FILE' )],
        [opt(contextfile), meta('FILE'), type(atom), shortflags([x]), longflags(['context']),
         help( 'context definition FILE' )],
        [opt(conditionsfile), meta('FILE'), type(atom), shortflags([c]), longflags(['conditions']),
         help( 'condition definition FILE' )],
        [opt(selftest), type(boolean), default(false), shortflags([s]), longflags(['selftest']),
         help( 'run self tests on startup' )],
        [opt(token), meta('TOKEN'), type(atom), shortflags([t]), longflags(['token']),
         help( 'policy administration requests must cite TOKEN' )],
        [opt(verbose), type(boolean), default(false), shortflags([v]), longflags(['verbose']),
         help( 'show all messages' )]
]).

:- dynamic epp_options/1.
epp_options([]).

epp(Port) :-
	create_epp_log,
	http_server(http_dispatch, [port(Port)]),
	format('EPP server started~n'),
	epp_log_gen(epp_start, success),
	param:server_sleeptime(S), my_sleep(S).

epp(Port,Token) :-
	param:setparam(admin_token,Token),
	epp(Port).


epp_with_args(Argv) :-
	% format('EPP Argv: ~q~n',[Argv]),
	% process the arguments
	epp_opt_spec(OptSpec),
	catch(
	    opt_parse(OptSpec,Argv,Opts,_Positionals),
	    E, writeln('error in command line arguments')),
	!,
	(   nonvar(E)
	->  halt(1)
	;   retractall(epp_options(_)), assert(epp_options(Opts)),
	    % format('Opts: ~q~nPositionals: ~q~n',[Opts,Positionals]),
	    memberchk(eppportnumber(EPort),Opts)
	),

	(   var(EPort)
	->  param:eppapi_port(EPort)
	;   true
	),

	(   memberchk(verbose(true),Opts)
	->  param:setparam(verbose,on) % turns on verbose globally
	;   param:setparam(verbose,off)
	),

	(   memberchk(selftest(true),Opts) % currently ignored
	->  param:setparam(self_test,on) % turns on self_test globally
	;   param:setparam(self_test,off)
	),

	(   ( memberchk(token(Token),Opts), atom(Token) )
	->  param:setparam(epp_admin_token,Token)
	;   true
	),

	format('EPP server starting on port ~d~n',[EPort]),
	create_epp_log,
	http_server(http_dispatch, [port(EPort)]),
	format('Epp server started~n'),
	epp_log_gen(epp_start, success),

	% run self-test here if turned on in param or command line

	(   memberchk(erpfile(Efile),Opts) ; true ),
	(   var(Efile)
	->  true % initial erp file not specified
	;   (   ( atom(Efile), exists_file(Efile) )
	    ->	erl:load_er_package(Efile,ERPname),
		param:setparam(current_erp,ERPname),
		format('ERP ~q loaded from ~a~n',[ERPname,Efile]),
		epp_log_gen(event_processing, erploadopt(Efile,ERPname,success))
	    ;   format('ERP file load error: ~a~n',[Efile]),
		epp_log_gen(event_processing, erploadopt(Efile,failure))
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

create_epp_log :- param:epp_logging(file), !,
	audit:gen_time_stamp(TS),
	atomic_list_concat(['LOG/epp_log','_',TS],LogFile),
	format('EPP log file: ~w~n',LogFile),
	open(LogFile,append,LogStream),
	param:setparam(epp_stream,LogStream).
create_epp_log.

epp_log_gen(LogEvent, LogData) :-
	(   param:epp_logging(file)
	->  param:epp_stream(Log),
	    audit:gen_time_stamp(TS),
	    format(Log, 'epp_log(~w, ~q, ~q).~n',[TS,LogEvent,LogData]),
	    flush_output(Log)
	;   true
	),
	audit_gen(LogEvent, LogData),
	!.


% ------------------------------------------------------------------------
% EPP TOP LEVEL INTERFACES
%   these are called by the Web APIs in module eppapi

load_erp(ERPfile,ERPname) :-
	erl:load_er_package(ERPfile,ERPname),
	activate_loaded_erp(ERPname),!.

unload_erp(ERPname) :-
	deactivate_loaded_erp(ERPname),
	erl:unload_er_package(ERPname),!.

% other interfaces called by eppapi are provided by epp_era


% ------------------------------------------------------------------------
authenticate_epp_token(Token) :-
	param:epp_admin_token(Token), !.
authenticate_epp_token(_) :- writeln('authentication error'), fail.

