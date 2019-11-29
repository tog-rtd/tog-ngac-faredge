:- module(pio,[policy_cmdstrs/2,
	       save_cmdstrs_to_file/2
	      ]).
% Input / Output of various policy representations

:- use_module(spld).


policy_cmdstrs(P,CmdStrs) :- % version 2
	findall( add(p(PC), c('PM')),
		 element(P,policy_class(PC)),
		 PCs ),

	findall( add(b(OA), p(PC)),
		 ( element(P,object_attribute(OA)), element(P,policy_class(PC)), assign(P,OA,PC) ),
		 OAs1 ),

	findall( add(a(UA), p(PC)),
		 ( element(P,user_attribute(UA)), element(P,policy_class(PC)), assign(P,UA,PC) ),
		 UAs1 ),

	findall( add(a(UA1), a(UA2)),
		 ( element(P,user_attribute(UA1)), element(P,user_attribute(UA2)), assign(P,UA1,UA2) ),
		 UAs2 ),

	findall( add(u(U), a(UA)),
		 ( element(P,user(U)), element(P,user_attribute(UA)), assign(P,U,UA) ),
		 Us ),

	%OC = 'File', Host = 'Host', Path = 'Path', % need to fix this
	findall( add(ob(O,OC,Inh,Host,Path,BType,OA)),
		 ( object(P,O,OC,Inh,Host,Path,BT,OA), type_map(BT,BType), element(P,object_attribute(OA)), assign(P,O,OA) ),
		 Os ),

	findall( [add(s(OPsetName),oc(ignored),a(UA)), AddOps, asg(s(OPsetName),b(OA))],
		 ( associate(P,UA,OPset,OA),
		   atomic_list_concat(['{',UA,'-[]-',OA,'}'],'',OPsetName),
		   findall(add(op(OP),s(OPsetName)), member(OP,OPset), AddOps) ),
		 NAssocs ), flatten(NAssocs,Assocs),

	append([PCs,UAs1,OAs1,UAs2,Us,Os,Assocs], CmdTerms),
	pmcmd:pmCmdStrs_CmdTerms(CmdStrs, CmdTerms),
	true.

type_map(object_attribute,b).

% load/save a single Prolog term from/to a file
load_term(FileName, Term) :- exists_file(FileName), !,
	open(FileName, read, File),
	read_term(File, Term, []),
	close(File).
load_term(FileName, _) :-
	ui:notify(FileName,'Does not exist.').

save_term(FileName, Term) :-
	open(FileName, write, File),
	writeq(File, Term), write(File,'.\n'),
	close(File), !.

save_cmdstrs_to_file(FileName, CmdStrs) :-
	open(FileName, write, File),
	save_cmdstrs(File, CmdStrs),
	close(File), !.

save_cmdstrs(_,[]).
save_cmdstrs(F,[Cmd|Cmds]) :-
	write(F,Cmd), nl(F),
	save_cmdstrs(F,Cmds).

%%

save_cmdterms_to_file(FileName, CmdTerms) :-
	open(FileName, write, File), write(File,'[\n'),
	save_cmdterms(File, CmdTerms), write(File, '].\n'),
	close(File), !.

save_cmdterms(_,[]).
save_cmdterms(F,[Term|Terms]) :-
	writeq(F,Term),
	(   Terms \== []
	->  write(F,',')
	;   true
	), nl(F),
	save_cmdterms(F,Terms).

% load a list of command terms [ term, term, ... ].
load_cmdterms_from_file(FileName, CmdTerms) :-
	load_term(FileName, CmdTerms).

% Reading and writing a PM state file
%
%   The PM state is a list of atoms that are each a CmdStr.
%   The PM state file is one CmdStr per line.
%

load_CmdTerms_from_CmdStrFile(F,CmdTerms) :-
	load_CmdStrs_from_file(F,CmdStrs),
	pmcmd:pmCmdStrs_CmdTerms(CmdStrs, CmdTerms).

% load_CmdStrs_from_file returns a list of commands from the file
%
load_CmdStrs_from_file(F,CmdStrs) :- exists_file(F), !,
	open(F,read,FS),
	load_CmdStrs_from_stream(FS,CmdStrs),
	close(FS).
load_CmdStrs_from_file(F, _) :-
	ui:notify(F,'Does not exist.').

% load_CmdStrs_from_stream returns a list of commands from a stream
%
% load_CmdStrs_from_stream(Stream,CmdStrs) :-
%	% need to deal with comment lines (beginning with #)
%	read_string(Stream, "\n", "\r\t ", End, S),
%	(   End \== -1
%	->  atom_codes(Sa,S),
%	    CmdStrs = [Sa|Ss],
%	    load_CmdStrs_from_stream(Stream,Ss)
%	;   CmdStrs = []
%	).

load_CmdStrs_from_stream(Stream,CmdStrs) :-
	read_string(Stream, "\n", "\r\t ", End, S),
	(   End \== -1
	->  ( % comment-out following 5 lines to keep comments/blank lines
%		( sub_string(S,0,1,_,"#") ; string_length(S,0) )
%	        % throw away comment and blank lines
%	    ->
%		load_CmdStrs_from_stream(Stream,CmdStrs)
%	    ;
		atom_codes(Sa,S),
		CmdStrs = [Sa|Ss],
		load_CmdStrs_from_stream(Stream,Ss)
	    )
	;   CmdStrs = []
	).

% pmCmdTerms_to_file converts a list of command terms to a list
% of command strings and writes them to a file
%
save_CmdTerms_to_CmdStrFile(F,CmdTerms) :-
	maplist(pmcmd:pmCmdStr_CmdTerm, CmdStrs, CmdTerms),
	save_CmdStrs_to_file(F,CmdStrs).

% pmCmdStrs_to_file writes a list of commands to a file
%
save_CmdStrs_to_file(F,CmdStrs) :-
	open(F,write,FS),
	save_CmdStrs_to_stream(FS,CmdStrs),
	close(FS).

% pmCmdStrs_to_stream writes a list of commands to a stream
%
save_CmdStrs_to_stream(_,[]) :- !.
save_CmdStrs_to_stream(Stream,[S|Ss]) :-
	write(Stream,S), nl(Stream),
	save_CmdStrs_to_stream(Stream,Ss).

display_policy(P) :- P\==none, policy(P,PC), !,
	format('policy(~q, ~q, [~n',[P,PC]),
	forall(element(P:PC,user(U)), format('  user(~q),~n',U)),
	forall(element(P:PC,user_attribute(UA)), format('  user_attribute(~q),~n',UA)),
	forall(element(P:PC,object(O)), format('  object(~q),~n',O)),
	forall(element(P:PC,object_attribute(OA)), format('  object_attribute(~q),~n',OA)),
	forall(element(P:PC,policy_class(PC)), format('  policy_class(~q),~n',PC)),
	forall(element(P:PC,connector(C)), format('  connector(~q)',C)),
	forall(assign(P:PC,E1,E2), format(',~n  assign(~q,~q)',[E1,E2])),
	forall(associate(P:PC,E1,Ops,E2), format(',~n  associate(~q,~q,~q)',[E1,Ops,E2])),
	format('~n]).~n').
