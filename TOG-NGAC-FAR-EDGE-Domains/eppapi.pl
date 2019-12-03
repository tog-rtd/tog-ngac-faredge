% ------------------------------------------------------------------------
% EPP WEB APIS
:- module(eppapi, []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).
:- use_module(epp).
:- use_module(epp_era).
:- use_module(audit).

% EPP WEB API
:- http_handler(root(epp), root_apis(eppapi), []).
:- http_handler(root('epp/'), api_unimpl, [prefix]).
:- http_handler(root(epp/load_er_package), epp_load_er_package, [prefix]).
:- http_handler(root(epp/unload_er_package), epp_unload_er_package, [prefix]).
:- http_handler(root(epp/activate_er_package), epp_activate_er_package, [prefix]).
:- http_handler(root(epp/deactivate_er_package), epp_deactivate_er_package, [prefix]).
:- http_handler(root(epp/report_event), epp_report_event, [prefix]).

eppapi([load_er_package, unload_er_package, activate_er_package, deactivate_er_package, report_event]).

%
% EPP API
%
%   load_er_package
%   unload_er_package
%   activate_er_package
%   deactivate_er_package
%   report_event
%

epp_load_er_package(Request) :- % load erp
	std_resp_prefix,
	catch(
	    http_parameters(Request,[erpfile(Efile,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_epp_token(Token) )
	->  writeln(failure),
	    epp_log_gen(event_processing, load_er_package(failure))
	;
	(   exists_file(Efile)
	->  load_erp(Efile,ERPname), % TODO add check for: all, none, grant, deny
	    format('ERP ~q loaded from ~a~n',[ERPname,Efile]),
	    writeln(success),
	    epp_log_gen(event_processing, load_er_package(Efile,ERPname,success))
	;   writeln('file error'),
	    writeln('failure'),
	    epp_log_gen(event_processing, load_er_package(Efile,failure))
	)).

epp_unload_er_package(Request) :- % unload erp
	std_resp_prefix,
	catch(
	    http_parameters(Request,[erpname(ERPname,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_epp_token(Token) )
	->  writeln(failure),
	    epp_log_gen(event_processing, unload_er_package(failure))
	;
	(   erl:er_package(ERPname,_)
	->  unload_erp(ERPname),
	    writeln(success),
	    epp_log_gen(event_processing, unload_er_package(ERPname,success))
	;   writeln('unknown policy'),
	    writeln('failure'),
	    epp_log_gen(event_processing, unload_er_package(ERPname,failure))
	)).

epp_activate_er_package(_).

epp_deactivate_er_package(_).

epp_report_event(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[event(Ename,[atom]),token(Token,[atom])]),
	    E,writeln('missing parameter')),
	!,
	(   ( nonvar(E); \+authenticate_epp_token(Token) )
	->  writeln(failure),
	    epp_log_gen(event_processing, report_event(failure))
	;
	(   report_event(Ename)
	->  writeln(success),
	    epp_log_gen(event_processing, report_event(Ename,success))
	;   writeln('failure'),
	    epp_log_gen(event_processing, report_event(Ename,failure))
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

use_valid_api(_) :-
	format('Use eppapi for EPP interaction~n').

std_resp_prefix :- format('Content-type: text/plain~n~n').

