% COMMAND INTERPRETER FOR BASIC USER INTERFACE
% and definition of the 'ngac' commands

:- module(command, [ tl/0, tl/1 ]).

:- use_module(param).
:- use_module(ui).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool command syntax
%
% there must be a syntax entry for every command form
% do not create a 0-ary command with the name "invalid" (see rd/2)
syntax(access(policy,(user,mode,object)),           admin).
syntax(admin,                                       admin).
syntax(advanced,                                    admin).
syntax(aoa(user),				    admin).
syntax(combine(p1,p2,p3),			    admin).
syntax(decl2imp(decl_file,imp_file),		    admin).
syntax(demo(demo_command),                          admin).
syntax(display_policy,                              admin).
syntax(display_policy(policy),                      admin).
syntax(dps(policy),                                 admin).
syntax(echo(string),                                admin).
syntax(export_commands(imp_file),                   admin).
syntax(halt,                                        admin).
syntax(help,                                        admin).
syntax(help(command),				    admin).
syntax(import(file_spec),                                     advanced).
syntax(import_policy(policy_file),	                      advanced).
syntax(inspect,                                     admin).
syntax(inspect(item),                               admin).
syntax(los(policy),                                 admin).
syntax(make,                                                  advanced).
syntax(minaoa(user),				    admin).
syntax(newpol(policyid),                            admin).
syntax(nl,                                          admin).
syntax(pmcmd,                                       admin).
syntax(proc(proc_id),                               admin).
syntax(proc(proc_id,step_or_verbose),		    admin).
syntax(quit,                                        admin).
syntax(regtest,                                               advanced).
syntax(reinit,                                                advanced).
syntax(script(file),                                admin).
syntax(script(file,step_or_verbose),		    admin).
syntax(selftest,					      advanced).
syntax(server(port),				    admin).
syntax(server(port,token),			    admin).
syntax(set,                                         admin).
syntax(set(name),				    admin).
syntax(set(name,value),				    admin).
syntax(status,				            admin).
syntax(step,                                                  advanced).
syntax(step(number_of_steps),				      advanced).
syntax(store(pol_id),				              advanced).
syntax(traceoff,					      advanced).
syntax(traceon,					              advanced).
syntax(traceone,					      advanced).
syntax(userlos(policy,user),                        admin).
syntax(version,				            admin).
syntax(versions,				    admin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool command semantics
%
% optional static semantics entry, e.g., used to check command arguments
% distinct from syntax so syntax can be called separately
semantics(access(P,(U,M,O))) :- !, ground(P), ground(U), ground(M), ground(O).
semantics(aoa(U)) :- !, ground(U).
semantics(combine(P1,P2,P3)) :- !, atom(P1), atom(P2), atom(P3).
semantics(decl2imp(Dfile,Ifile)) :- !, atom(Dfile), atom(Ifile).
semantics(demo(C)) :- !, ground(C).
semantics(display_policy(P)) :- !, atom(P).
semantics(dps(P)) :- !, ground(P).
semantics(echo(S)) :- !, atomic(S).
semantics(export_commands(C)) :- atom(C).
semantics(help(C)) :- !, ground(C).
semantics(import(FS)) :- !, functor(FS,F,1), (F==policy ; (F==model ; (F==pm ; F==database))).
semantics(import_policy(P)) :- atom(P).
semantics(import_pm(PM)) :- atom(PM).
semantics(import_model(M)) :- atom(M).
semantics(inspect(I)) :- nonvar(I).
semantics(los(P)) :- !, ground(P).
semantics(minaoa(U)) :- !, ground(U).
semantics(newpol(ID)) :- !, ground(ID).
semantics(newpol(T,ID)) :- !, ground(ID), ground(T).
semantics(proc(P)) :- !, atom(P).
semantics(proc(P,Opt)) :- !, atom(P), (Opt==step;Opt==s;Opt==verbose;Opt==v). % other opts can be added
semantics(script(F)) :- !, atom(F).
semantics(script(F,Opt)) :- !, atom(F), (Opt==step;Opt==s;Opt==verbose;Opt==v). % other opts can be added
semantics(server(Port)) :- !, integer(Port).
semantics(server(Port,Token)) :- !, integer(Port), atom(Token).
semantics(set(N)) :- !, atom(N).
semantics(set(N,V)) :- !, atom(N), ground(V).
semantics(step(N)) :- !, (integer(N) ; N == break), !.
semantics(store(P)) :- !, atom(P).
semantics(userlos(P,U)) :- !, ground(P), ground(U).
semantics(_). % succeed for all other commands

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command help strings
%   all strings for a given key are displayed when key is given as an
%   argument to the help command, e.g., "help(assess)"
%
%   help(Key,    HelpString)
help(access,    'Under policy, user can access in mode the object.').
help(access,	'Arg1 is a policy name.').
help(access,    'Arg2 is and access triple, "(User, Mode, Object)".').

help(admin,     'Switch to admin user mode, limiting available commands to admin command set.').
help(advanced,  'Switch to advanced user mode, enabling all commands.').

help(aoa,	'all object attributes for user in current policy and policy class').
help(aoa,       'Arg is user identifier.').

help(combine,	'Arg1 and Arg2 are the names of currently loaded declarative policy specs.').
help(combine,	'Arg3 is name of a new policy spec that is the combination of the first two.').

help(decl2imp,	'Arg1 is name of input file containing declarative policy spec.').
help(decl2imp,	'Arg2 is name of output file to contain imperative policy spec.').

help(demo,	'Run canned demos.'). % command for running canned demos of different features
help(demo,      'Arg is demo identifier.').

help(display_policy, 'Display the current policy,').
help(display_policy, 'or another currently loaded policy named by Arg.').

help(dps,       'Show derived privileges of the specified policy').
help(dps,	'Arg is a policy name').

help(echo,      'echo a (single-quoted) string argument.').

help(export,    'export a specified model(model_id), policy(type,attrs) or database(db_id)').

help(export_policy, '"export" a policy for consumption by external tools').
help(export_model, '"export" a model for comsumption by external tools').

help(halt,	'Leave NGAC command loop and Prolog.').

help(help,	'"help" with no argument lists the legal command forms.').
help(help,	'With a command name as argument it provides help on that command.').

help(import,    'import a specified policy policy(file), pm(file).').

help(import_policy, '"import" a declarative policy from a file.').
help(import_pm, '"import" a policy in PM imperative commands from a file').

help(inspect,	'Inspect values of internal structures or variables based on arg.').
help(inspect,	'arg options: settings, xml, str, prm, current or other structures.').
help(inspect,	'arg: target(<target>,<element>) will show intermediate facts.').

help(los,       'Show logical object system of the specified policy').
help(los,	'Arg is a policy name').

help(make,	'Recompile changed source files.').

help(newpol,	'Create a new policy as the "current policy".').
help(newpol,	'If two arguments (currently unimplemented) the first is policy type.').
help(newpol,	'Last argument is the policy ID.').

help(nl,        'Write a newline to the console.').

help(pmcmd,	'Enter PM server command mode.').

help(proc,	'Run a stored NGAC command procedure.').
help(proc,	'Arg 1 is a procedure identifier.').
help(proc,      'Arg 2 (optional) is "step" or "verbose".').

help(quit,	'Terminate the NGAC top-level command loop or a command script; stay in Prolog.').

help(regtest,   'Run regression tests.').

help(reinit,	'Re-initialize.').


help(script,	'Run a NGAC command script from a file.').
help(script,	'Arg 1 is the file name.').
help(script,	'Arg 2 (optional) is "step" or "verbose".').

help(selftest,  'Run self tests.').

help(server,	'Start the lightweight policy server.').
help(server,    'Arg1 is the port number.').
help(server,    'Arg2 (optional) is an admin token.').

help(set,	'With no argument displays all settable parameters.').
help(set,	'Arg 1 is name of a paramater. If only one arg, display its value.').
help(set,	'Arg 2, if present, is the value to set.').
help(set,       'Settable: cache, debug, initialize, statusprt, self_test, regression_test, verbose.').

help(status,	'Display NGAC system status.').

help(step,	'"step" with no argument steps engine one cycle.').
help(step,	'With an integer argument it steps the engine that number of cycles.').


help(traceoff,  'Turn Prolog tracing off.').
help(traceon,   'Turn Prolog tracing on.').
help(traceone,	'Turn Prolog tracing on for one NGAC command.').

help(userlos,   'Show the logical object system under policy for user.').
help(userlos,	'Arg 1 is policy name.').
help(userlos,   'Arg 2 is user name.').

help(version,	'Show current version number.').
help(versions,	'Show past versions with descriptions and current version.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% do the command, should be one for every implemented valid command form
% known broken or unimplemented commands should just "fail." straightaway
% interactive_do provides an appropriate message for interactive use of
% a command that is known to be invalid because it fails syntax or
% semantics check, does not have a do entry, or fails in do.
% (Would be better to distinguish between unimplemented and failed do,
% which is what the fail_act in tl is for.)
% As it is now, commands with an entry in do that fail are reported
% in the interactive_do as unimplemented commands.
%
do(access(P,(U,M,O))) :- !,
	(   spld:access_check(P,(U,M,O))
	->  writeln(grant)
	;   writeln(deny)
	).
do(admin) :- !, retractall(user_mode(_)), assert(user_mode(admin)).
do(advanced) :- !, retractall(user_mode(_)), assert(user_mode(advanced)).
do(aoa(U)) :- !, param:current_policy(P), spld:policy(P,PC),
	spld:aoa(P,U,PC,AOA), ui:display_list(AOA).
do(combine(P1,P2,Presult)) :- !,
	spld:compose_policies(P1,P2,Presult),
	true.
do(decl2imp(D,I)) :- !,
	 % same as import_policy+export_commands w/o making current policy
	spld:decl2imp(D,I).
do(demo(C)) :- !, perform_demo(C).
do(display_policy) :- !, do(display_policy(current_policy)).
do(display_policy(current_policy)) :- !, param:current_policy(P), do(display_policy(P)).
do(display_policy(P)) :- !, pio:display_policy(P).
do(dps(P)) :- !, %param:current_policy(P), % spld:policy(P,PC),
	spld:policy_dps(P,DPS), ui:display_list(DPS).
do(echo(S)) :- !, writeln(S).
do(export(commands,CmdFile)) :- !,
	param:current_policy(PolicyName),
	do( export_commands(PolicyName,CmdFile) ).
do(export_commands(PolicyName,CmdFile)) :-
	spld:save_as_cmds(PolicyName,CmdFile).
do(help) :- help.
do(help) :- !.
do(help(C)) :- !, show_help(C).
do(import(pm(PM))) :- do(import_pm(PM)).
do(import(policy(P))) :- do(import_policy(P)).
do(import_pm(PM)) :- % import a PM imperative command file
	pio:load_CmdTerms_from_CmdStrFile(PM,CmdTerms),
	% create policy form
	spld:cmdTerms2policy(CmdTerms,Policy),
	% make it a queryable policy
	spld:unpack_policy(Policy).
do(import_policy(Pfile)) :-  % import declarative policy
	spld:load_decl_policy(Pfile,PolicyName),
	ui:notify('Policy loaded',PolicyName),
	do( newpol(PolicyName) ).
do(inspect) :- !, writeln('inspect(opt). options: settings').
do(inspect(Item)) :- !, inspect(Item).
do(los(P)) :- !, spld:los(P,LOS), ui:display_list(LOS).
do(make) :- !, make.
do(minaoa(U)) :- !, param:current_policy(P), spld:policy(P,PC),
	spld:min_aoa(P,U,PC,MAOA), ui:display_list(MAOA).
do(newpol(P)) :- !, spld:set_current_policy(P).
do(nl) :- nl.
do(pmcmd) :- !, (interactive(true) -> tl(pmcmd) ; true).
do(proc(Pid)) :- !, do(proc(Pid,none)).
do(proc(Pid,Opt)) :- !, procs:proc(Pid,Proc), param:prompt_string(P),
	retractall(interactive(_)), assert(interactive(false)),
	run_commands(P,Proc,Opt),
	retractall(interactive(_)), assert(interactive(true)).
do(quit) :- !.
do(halt) :- !, halt.
do(regtest) :- !, ngac:regression_test_all.
do(reinit) :- !, spld:re_init.
do(script(F)) :- !, param:prompt_string(P), run_command_script(P,F,none).
do(script(F,Opt)) :- !, param:prompt_string(P), run_command_script(P,F,Opt).
do(selftest) :- !, ngac:self_test_all, /* others ... */ true.
do(set) :- !, param:settable_params(Ps), forall(member(P,Ps),do(set(P))). % display all settable params
do(set(P)) :- param:settable_params(Ps), member(P,Ps), !, % display a settable param
	Q =.. [P,V], call(param:Q), format('~a=~a~n',[P,V]).
do(set(_)) :- !, writeln('Unknown parameter name').
do(set(debug,V)) :- (V == on ; V == off), !, do(debug(V)).
do(set(statusprt,V)) :- (V == on ; V == off), !, do(statusprt(V)).
do(set(self_test,V)) :- (V == on ; V == off), !, do(self_test(V)).
do(server(Port)) :- !, server:server(Port).
do(server(Port,Token)) :- !, server:server(Port,Token).
do(set(initialize,V)) :- (V == on ; V == off), !,
	retract(param:initialize(_)), assert(param:initialize(V)).
do(set(regression_test,V)) :- (V == on ; V == off), !, param:setparam(regression_test,V).
do(set(verbose,V)) :- (V == on ; V == off), !, param:setparam(verbose,V).
do(set(policy,P)) :- !, do(newpol(P)).
% add cases for other parameter settings here
do(set(_,_)) :- !,
	writeln('Unknown parameter name or illegal parameter value').
do(status) :- param:ngac_name(N,_), write(N),
	writeln(' system status:'), ngac_status(status(S)), write_status(S).
do(traceon) :-	retractall(tracing(_)), assert(tracing(on)), trace.
do(traceone) :-	retractall(tracing(_)), assert(tracing(set)).
do(traceoff) :- retractall(tracing(_)), assert(tracing(off)), notrace.
do(userlos(P,U)) :- spld:user_los(P,U,V,E),
	write('V='), ui:display_list(V,''), write('E='), ui:display_list(E,'').
do(version) :- !,
	param:ngac_version(Cur), param:ngac_current_version_description(Desc),
	format('Current version: ~a: ~a~n',[Cur,Desc]).
do(versions) :- !,
	forall(param:ngac_version(V,D), format('~t~17|~a: ~a~n',[V,D])),
	do(version).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic tracing/1, user_mode/1, interactive/1.

% tracing
% values: on, off, set, and one
%         set changes to one in mid next iteration
%         one changes to off after the next command is run
tracing(off).

% user_mode
% values: admin or advanced
user_mode(advanced).

% interactive
% true when reading from user interaction
interactive(true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top-level command loop
%
% tl(CommandSet)
%
tl :- tl(ngac).


tl(ngac) :-
	param:user_level(Ulev), retractall(user_mode(_)), assert(user_mode(Ulev)),
	param:prompt_string(Prompt),
	banner,
	repeat,
	        pre_act, rd(Prompt,C), mid_act(C),
		(   interactive_do(ngac,C)
		->  true
		;   fail_act
		),
		post_act,
	(C == quit, ! ; fail).


banner :-
	param:ngac_version(V), param:name_string(NGAC_name),
	format('~n~a version ~a~n',[NGAC_name,V]),
	nl.

pre_act :- % do before reading the command
	true.
mid_act(_) :- % do after reading the command but before do-ing it
	(   tracing(set)
	->  retractall(tracing(_)),
	    assert(tracing(one)),
	    trace
	;   true
	).
post_act :- % do after performing the command or after fail_act
	(   tracing(one)
	->  retractall(tracing(_)),
	    assert(tracing(off)),
	    notrace
	;   true
	),
	(param:statusprt(on) -> do(status);true),
	nl, !.
fail_act :- % do when a command fails
	(   tracing(one)
	->  retractall(tracing(_)),
	    assert(tracing(off)),
	    notrace
	;   true
	),
	param:msg_failed_command(M),
	ui:notify('interactive',M).

interactive_do(_,invalid) :- !, unimplemented_command.
interactive_do(CS,C) :- param:prompt_string(CS), !, do(C).
interactive_do(CS,C) :-	 atom(CS), DO =.. [CS,C], !, call(CS:DO).
interactive_do(_,_) :- unimplemented_command.

unimplemented_command :- param:msg_unimplemented_command(M), writeln(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read and validate a command:
% execute a Prolog goal preceded by :- or ?-
% or check whether a valid NGAC command
% return invalid if not found or fails checks
%
rd(Prompt,C) :-
	atom_concat(Prompt,'> ',FullPrompt),
	read_history(h, '!h', [], FullPrompt, C, _Bindings),
	nonvar(C), % nonvar instead of ground to allow Prolog goals w/vars
	(   (C=..[:-,P];C=..[?-,P]) % command is a Prolog goal
	->  call(P), nl, !, fail    % bypass other goals in tl, repeat
	;   chk_command(Prompt,C)          % check the command, fail to 'invalid'
	), !.
rd(_,invalid).

chk_command(CommandSet,C) :-
	param:prompt_string(Prompt),
	(   CommandSet == Prompt
	->  syntax_chk(C),
	    semantics(C)
	;   Check =.. [cmd,C,_,_],
	    clause(CommandSet:Check,true)
	).

syntax_chk(C) :-
	functor(C,F,A), functor(Sig,F,A), user_mode(M),
	(   M == advanced
	->  syntax(Sig,_)
	;   syntax(Sig,M)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command scripts
%

run_command_script(CmdSet,F,Opt) :-
	(   access_file(F,read)
	->  (
	        read_file_to_terms(F,Commands,[]),
	        (   Opt == verbose
	        ->  param:msg_script_read(Mread), writeln(Mread),
		    ui:display_list(Commands,1),
	            param:msg_running_script(Mrun), writeln(Mrun)
	        ;   true
	        ),
	        run_commands(CmdSet,Commands,Opt)
	    )
	;
	    format('can''t find file "~a"~n', F)
	), !.

run_commands(_,[],_) :- !.
run_commands(CmdSet,[C|Cs],Opt) :-
	(
	    (	(Opt == step ; Opt == s)
	    ->	format('~n> ~q. ?', C), flush_output, readln(_)
	    ;	(Opt == verbose ; Opt == v)
	    ->	format('> ~q.~n', C)
	    ;	true
	    ),
	    (   (C=..[:-,P] ; C=..[?-,P]) % command is a Prolog goal
	    ->  call(P)
	    ;   % ground(C),
	        chk_command(CmdSet,C),          % check the command, fail to 'invalid'
		(   param:prompt_string(CmdSet)
		->  do(C)
		;   atom(CmdSet), DO =.. [CmdSet,C], call(CmdSet:DO)
		)
	    )
	    ;   format('~q : ',[C]),
		param:msg_failed_command(CM), writeln(CM),
		param:msg_script_aborted(SM), writeln(SM),
		Abort=true
	),
	((C == quit; Abort == true), ! ; run_commands(CmdSet,Cs,Opt)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command procedures
%   command procedures for miscellaneous commands, in some cases
%   procedures here may be temporary until they are moved to an
%   appropriate module

help :-
	user_mode(M), !,
	writeln('<command> ::='),
	(   M == advanced
	->  syntax(Sig,_)
	;   syntax(Sig,M)
	),
	write('  '), write_canonical(Sig), nl, fail.

show_help(C) :-
	C =.. [Command|_], % use only the command name, ignore args
	(   help(Command,_)
	->  nl, show_help_strings(Command)
	;   format('No help for command "~q"~n', Command)
	).

show_help_strings(Command) :-
	help(Command,HelpString), format('  ~a~n',HelpString), fail.
show_help_strings(_).

% inspection - for development and test
inspect(settings) :- !, do(set).
% add other inspect clauses here
%% - inspect(graph) :- !, graphmanager:getGraph(G),graphmanager:printGraph(G).
inspect(_) :- writeln('inspect: Unknown parameter name or illegal parameter value').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% status
%

ngac_status(status(NGAC)) :-
	NGAC = 'Blah blah blah'.

write_status(S) :- writeln(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% demos
%   to show-off implemented portions of functionality
%   insert perform_demo clauses for specific arguments following comment

perform_demo(X) :- unimpl_d(X).

unimpl_d(X) :- format('Unimplemented demo command: ~q~n',[X]).

