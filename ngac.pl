% Next Generation Access Control policy tool

:- module(ngac, [ngac/0,ngac/1,ngac/4,ngac_server/0]).
:- use_module([
       param,command,common,pio,policies,
       test,procs,pmcmd,spld,server,sessions,
       n_audit
   ]).

%=====================================================================
% NOTES and TODOs:
%
%=====================================================================

:- style_check(-singleton).

% :- initialization(ngac).
%

:- set_prolog_flag(verbose, silent).

% These are the main entry points to the system
% Other special entry points may also be defined here
%
ngac :- % most typical entry
	get_command_args(_Argv),
	ngac(_,_,_,_).
ngac :- halt(1).

% can be invoked with directives: (could do to allow a set of directives)
ngac(self_test) :- !, ngac(on,off,on,_).
ngac(regression_test) :- !, ngac(off,on,on,_).
ngac(no_initial) :- !, ngac(off,off,off,_).
ngac(verbose) :- !, ngac(_,_,_,on).

ngac(Selftest,Regression,Init,Verbose) :-
	(   var(Selftest) -> param:self_test(Selftest) ; true ),
	(   var(Regression) -> param:regression_test(Regression) ; true),
	(   var(Init) -> param:initialize(Init) ; true ),
	(   var(Verbose) -> param:verbose(Verbose) ; true ),

	(   Verbose == on
	-> format('self_test=~a regression_test=~a initialize=~a verbose=~a~n',
		  [Selftest,Regression,Init,Verbose])
	; true),

	(   Init == on
	-> initialize_all
	; true ),

	(   Selftest == on
	->  self_test_all
	;   true ),

	(   Regression == on
	->  regression_test_all
	;   true ),

	% guitracer,

	param:prompt_string(Prompt),
	command:tl(Prompt). % run the top-level ngac command interpreter

ngac_server :-
	get_command_args(Argv),
	initialize_all,
	server:server_with_args(Argv).

get_command_args(Argv) :-
	current_prolog_flag(argv, Argv),
	% format('Argv: ~q~n',[Argv]),
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialization
%
initialize_all :-
	% initialize all subsystems and modules requiring it

	spld:init,
	% ...

	true.

% Test
%
self_test_all :-
	test:self_test,
	true.

regression_test_all :-
	test:regression_test,
	true.
