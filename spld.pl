% Security Policy Language Definitions

:- module(spld,[init/0, los/2, user_los/4, aoa/4, min_aoa/4, is_visible/4,
		policy/2, element/2, assign/3, associate/4,
		load_decl_policy/2, load_decl_policy_immediate/2, save_decl_policy/2,
		object_oattribute/2, object/2, object/8,
		user_uattribute/2, user/2,
	        access_check/2, compose_policies/3,
	        decl2imp/2, imp2decl/3,
		cmdTerms2policy/2,
		set_current_policy/1
	       ]).

:- use_module(policies).
:- use_module(pio).
:- use_module(param).

:- include('TEST/spld_test').

% The lightweight policy model
%
% Cache of loaded policies as asserted clauses
% policy(PolicyName, PolicyRoot)
%
% element(PolicyName:PolicyRoot, Element)
%   where Element is: user(_), user_attribute(_)
%     object(_), object_attribute(_), policy_class(_),
%     opset(OpSetName, OpList), operation(OpName, OpInfo), operation(OpName),
%     connector('pm') is an element of every policy
%     assign(PolicyName:PolicyRoot, PolicyElement1, PolicyElement2)
%     associate(PolicyName:PolicyRoot, UserAttr, OpSetName, ObjectAttr)

:- dynamic policy/2, element/2, assign/3, associate/4.
% policy(PolicyName, PolicyClass)
%

policy_elements([user,user_attribute,object,object_attribute,policy_class,
		 operation,opset,composed_policy,assign,associate,connector]).

policy_elements_args([user(_),user_attribute(_),
		      object(_),object(_,_,_,_,_,_,_),
		      object_attribute(_),policy_class(_),operation(_),operation(_,_),
		      opset(_,_),composed_policy(_,_,_),assign(_,_),associate(_,_,_),
		      connector(_)]).

init:- param:initialized(true), !. % already initialized
init :-
	forall( policies:policy(Pn,Pr,Pg), unpack_policy( policy(Pn,Pr,Pg) ) ),
	param:setparam(initialized,true),
	true.

re_init :- un_init, init.

un_init :-
	clear_policy,
	param:setparam(initialized,false).

clear_policy :-
	retractall(policy(_,_)), retractall(element(_,_)),
	retractall(assign(_,_,_)), retractall(associate(_,_,_,_)),
	param:setparam(current_policy,none),
	true.

set_current_policy(Pname) :- atom(Pname), !,
	(   Pname == all
	->  clear_policy, Name=Pname
	;   ( Pname == allnc % no clear - used only for testing
	    ->  Name=all
	    ;	Name=Pname
	    )
	),
	param:setparam(current_policy,Name).

decl2imp(Dfile,Ifile) :-
	load_decl_policy(Dfile,PolicyName),
	save_as_cmds(PolicyName,Ifile).

save_as_cmds(PolicyName,CmdFile) :-
	pio:policy_cmdstrs(PolicyName,CmdStrs),
	(   param:verbose(on)
	->  ui:display_listq(CmdStrs,1)
	;   true
	),
	pio:save_cmdstrs_to_file(CmdFile,CmdStrs).

imp2decl(_Ifile,_Policy,_Dfile) :-
	% TO DO
	true.

load_decl_policy(Pfile,PolicyName) :-
	pio:load_term(Pfile,PolicyTerm),
	load_decl_policy_common(PolicyTerm,PolicyName).

load_decl_policy_immediate(PolicyAtom,PolicyName) :-
	read_term_from_atom(PolicyAtom,PolicyTerm,[]),
	load_decl_policy_common(PolicyTerm,PolicyName).

load_decl_policy_common(PolicyTerm,PolicyName) :-
	PolicyTerm = policy(PolicyName,PolicyRoot,PolicyElements),
	retractall(policies:policy(PolicyName,_,_)),
	assertz(policies:PolicyTerm),
	atom(PolicyName), atom(PolicyRoot), is_list(PolicyElements),
	unpack_policy(PolicyTerm).

save_decl_policy(Pfile,PolicyTerm) :-
	PolicyTerm = policy(_PolicyName,_PolicyRoot,_PolicyGraph),
	pio:save_term(Pfile,PolicyTerm),
	true.

cmdTerms2policy(_CmdTerms,_Policy) :- true.
	% UNIMPLEMENTED
%	cmdTerms_policyElts(CmdTerms,PolicyElements),
%	Policy = policy(PolicyName,PolicyRoot,PolicyElements),
%	true.

% cmdTerms_policyElts([],[]).
% cmdTerms_policyElts([Term|Terms],[Elt|Elts]) :-	true.

%

unpack_policy(policy(PolicyName,PolicyRoot,PolicyElements)) :-
	purge_policy(PolicyName),
	assertz( policy(PolicyName,PolicyRoot) ),
	unpack_policy_elements(PolicyName:PolicyRoot,PolicyElements), !.

unpack_policy_elements(_,[]).
unpack_policy_elements(PName,[PolElt|PolElts]) :-
	PolElt = assign(I,A), !,
	assertz( assign(PName,I,A) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[PolElt|PolElts]) :-
	PolElt = associate(I,M,A), !,
	assertz( associate(PName,I,M,A) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[PolElt|PolElts]) :-
	policy_elements_args(PolEltsArgs),
	memberchk(PolElt,PolEltsArgs),
	assertz( element(PName,PolElt) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[_,PolElts]) :-
	unpack_policy_elements(PName,PolElts).

purge_policy(PolicyName) :-
	retractall(policy(PolicyName:_,_)),
	retractall(element(PolicyName:_,_)),
	retractall(assign(PolicyName:_,_,_)),
	retractall(associate(PolicyName:_,_,_,_)),
	retractall(policy(PolicyName,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_oattribute(P,O) :- element(P,object(O)), !.
object_oattribute(P,O) :- element(P,object(O,_,_,_,_,_,_)), !.
object_oattribute(P,O) :- element(P,object_attribute(O)).

object(P,O) :- element(P,object(O)).
object(P,O) :- element(P,object(O,_,_,_,_,_,_)).

object(P,O,Oclass,Inh,Host,Path,BaseType,BaseName) :-
	element(P,object(O,Oclass,Inh,Host,Path,BaseType,BaseName)), !.
% default path for object element of the form object(<name>) :
object(P,O,file,no,localhost,Path,object_attribute,BaseName) :-
	element(P,object(O)), assign(P,O,BaseName), atom_concat('FILES/',O,Path).

user_uattribute(P,U) :- user(P,U).
user_uattribute(P,U) :- element(P,user_attribute(U)).

user(P,U) :- element(P,user(U)).

policy_class(P,PC) :- element(P,policy_class(PC)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Is a given object o assigned directly or indirectly to a
% given object attribute in the policy
%   is_contained_in_oa(Policy,Object,ObjectAttribute)
%
is_contained_in_oa(P:Pr, O, O) :- atom(P), atom(O),
	policy(P,Pr),
	object(P:Pr,O), !.
is_contained_in_oa(P:Pr, O, OA) :- atom(P), atom(OA), % policy(P,Pr),
	is_contained_in_oa1(P:Pr, O, OA).

is_contained_in_oa1(P:Pr, O, OA) :- atom(P), atom(OA), % defensive checks could be removed
	policy(P,Pr),
	assign(P:Pr,O,OA), object_oattribute(P:Pr,OA).
is_contained_in_oa1(P:Pr, O, OA) :- atom(P), atom(OA),
	policy(P,Pr),
	assign(P:Pr,OA1,OA),
	is_contained_in_oa1(P:Pr,O,OA1).

is_contained_in_ua(P:Pr, UorUA, UA) :- atom(P),
	policy(P,Pr), atom(UA),
	assign(P:Pr,UorUA,UA),
	(   element(P:Pr,user(UorUA)); element(P:Pr,user_attribute(UorUA)) ), !.
is_contained_in_ua(P:Pr, UorUA, UA) :- atom(P),
	policy(P,Pr), atom(UA),
	assign(P:Pr,UA1,UA),
	is_contained_in_ua(P:Pr,UorUA,UA1).
% SAVED
%is_contained_o_in_pc(P:Pr, O, PC) :- atom(P), atom(O), atom(PC), !,
%	policy(P,Pr), policy_class(P:Pr,PC), object(P:Pr,O),
%	assign(P:Pr,OA,PC), object_oattribute(P:Pr,OA),
%	is_contained_in_oa(P:Pr, O, OA), !.
%is_contained_o_in_pc(P:Pr, O, PC) :- atom(P), atom(O), var(PC), !,
%	fail, % insufficiently instantiated, may not need this case
%	policy(P,Pr), object_oattribute(P:Pr,O),
%	assign(P:Pr,O,OA), object_oattribute(P:Pr,OA),
%	% TODO see above
%	true.

% E can be an object or an object_attribute in this predicate
% see is_contained_o_in_pc (a version specific to objects)
is_contained_in_pc(P:Pr, E, PC) :- atom(P), atom(Pr), atom(E), atom(PC), !,
	policy(P,Pr), policy_class(P:Pr,PC), object_oattribute(P:Pr,E),
	assign(P:Pr,OA,PC), object_oattribute(P:Pr,OA),
	(   E=OA
	->  !
	;   is_contained_in_oa1(P:Pr,E,OA), !
	).

% NEW for 'all' policy composition in policy_dpc and qualified_...
is_contained_u_in_ua(P:Pr, U, UA) :- assign(P:Pr,U,UA), !.
is_contained_u_in_ua(P:Pr, U, UA) :-
	assign(P:Pr,UA1,UA), user_uattribute(P:Pr,UA1),
	is_contained_u_in_ua(P:Pr,U,UA1).

is_contained_o_in_pc(P:Pr, O, PC) :- atom(P), atom(O), atom(PC), !,
	policy(P,Pr), policy_class(P:Pr,PC), object(P:Pr,O),
	is_contained_in_pc(P:Pr,O,PC).

is_contained_u_in_pc(P:Pr, U, PC) :- atom(P), atom(U), atom(PC), !,
	policy(P,Pr), policy_class(P:Pr,PC), user(P:Pr,U),
	assign(P:Pr,UA,PC), user_uattribute(P:Pr,UA),
	(   U=UA
	->  !
	;   is_contained_u_in_ua(P:Pr, U, UA), !
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_contained_star(+P, +E1, +E2)
is_contained_star(P:Pr,E,E) :- atom(P), policy(P,Pr), atom(E), object_oattribute(P:Pr,E), !.
is_contained_star(P:Pr,E1,E2) :- is_contained_plus(P:Pr,E1,E2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_contained_plus(+P, +E1, +E2)
is_contained_plus(P:Pr,E1,E2) :- is_contained_in(P:Pr,E1,E2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_contained_in(+P, +E1, +E2)
is_contained_in(P:Pr, E1, E2) :-
	atom(P), policy(P,Pr), atom(E1), atom(E2),
	assign(P:Pr,E1,E2), !.
is_contained_in(P:Pr, E1, E2) :-
	atom(P), policy(P,Pr), atom(E1), atom(E2),
	assign(P:Pr, E1p, E2),
	is_contained_in(P:Pr, E1, E1p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a derived privilege of the combined policy
% for all policy classes having E there is a solution for (U,AR,E)
% As it is policy_dpc cannot be used as an enumerator as could policy_dp
% This breaks los() and user_los() until they are correspondingly
% updated. Fortunately they are non-essential in the current usage.
%
% Policy composition algorithm variations for policy 'all' query a
% subset of the currently loaded policies based on one of the qualifying
% conditions:
%   variant #1 p_o all policies having the object O
%   variant #2 p_uo all policies having the user U and the object O
%   variant #3 pc_o all policy classes having the object O
%   variant #4 pc_uo all policy classes having the user U and object O
%
% These are based on how the policies to be consulted are qualified.
% All policies currently loaded are checked for qualification.
% The normal policy_dpc is invoked for all qualified policies.
%
%
% Depending on qualification of P and PC:
%   a policy qualifies if it governs O and U qualified_p_for_uo
%   a policy qualifies if it governs O qualified_p_for_o
%
%   a pc qualifies if it governs O and U qualified_pc_for_uo
%   a pc qualifies if it governs O qualified_pc_for_o
%

policy_dpc(all:p_o,(U,AR,O)) :- !,
	forall( (spld:policy(P,Pr), qualified_p_for_o(P:Pr,O)),
		 policy_dpc(P:Pr,(U,AR,O)) ).
policy_dpc(all:p_uo,(U,AR,O)) :- !,
	forall( (spld:policy(P,Pr), qualified_p_for_uo(P:Pr,U,O)),
		 policy_dpc(P:Pr,(U,AR,O)) ).

% these 4 'x' versions generate diagnostic output
policy_dpc(all:xp_o,(U,AR,O)) :- !,
	findall(P:Pr, (spld:policy(P,Pr)), Policies),
	format('all policies: ~q~n',[Policies]),

	findall(P:Pr, (member(P:Pr,Policies),qualified_p_for_o(P:Pr,O)), QPolicies),
	format('qualifying policies: ~q~n',[QPolicies]),
	findall(R, (member(P:Pr,QPolicies), (policy_dpc(P:Pr,(U,AR,O))->R=grant;R=deny)), QPresults),
	format('Results: ~q~nOverall: ',[QPresults]),
	(   memberchk(deny,QPresults) -> writeln(deny) ; writeln(grant) ).
policy_dpc(all:xp_uo,(U,AR,O)) :- !,
	findall(P:Pr, (spld:policy(P,Pr)), Policies),
	format('all policies: ~q~n',[Policies]),

	findall(P:Pr, (member(P:Pr,Policies),qualified_p_for_uo(P:Pr,U,O)), QPolicies),
	format('qualifying policies: ~q~n',[QPolicies]),
	findall(R, (member(P:Pr,QPolicies), (policy_dpc(P:Pr,(U,AR,O))->R=grant;R=deny)), QPresults),
	format('Results: ~q~nOverall: ',[QPresults]),
	(   memberchk(deny,QPresults) -> writeln(deny) ; writeln(grant) ).
policy_dpc(all:xpc_o,(U,AR,O)) :- !,
	forall( (spld:policy(P,Pr),
		 findall(PC, policy_class(P:Pr,PC), PCs),
		 member(Pc,PCs), qualified_pc_for_o(P:Pr,Pc,O)),
		(format('policy ~q, policy classes ~q~n',[P,PCs]),
		 policy_dpc(P:Pr,(U,AR,O))) ).
policy_dpc(all:xpc_uo,(U,AR,O)) :- !,
	forall( (spld:policy(P,Pr),
		 findall(PC, policy_class(P:Pr,PC), PCs),
		 member(Pc,PCs), qualified_pc_for_uo(P:Pr,Pc,U,O)),
		(format('policy ~q, policy classes ~q~n',[P,PCs]),
		 policy_dpc(P:Pr,(U,AR,O))) ).
% the core test
policy_dpc(P:Pr,(U,AR,E)) :- atom(P),
	% All of the policy classes within the specified policy that
	% govern the entity E will be consulted.
	policy(P,Pr),
	findall(PC, (policy_class(P:Pr,PC), is_contained_in_pc(P:Pr,E,PC)), PCs),
	(   PCs == []
	->  !, fail
	;   foreach( member(Pc,PCs), policy_dpc_body(P:Pr,(U,AR,E),Pc))
	).

policy_dpc_body(P:Pr,(U,AR,E),PC) :-
	associate(P:Pr,UA,ARs,AT),
	is_contained_in(P:Pr,UA,PC),
	is_contained_in_pc(P:Pr,AT,PC),
	is_contained_in_ua(P:Pr,U,UA),
        is_contained_in_oa(P:Pr,E,AT),
	member(AR,ARs), !.
%
% Used in the construction of variations of policy_dpc
% in following predicates P, Pr, PC must be valid
%
qualified_p_for_u(P:Pr,U) :- atom(U), user(P:Pr,U).
qualified_p_for_o(P:Pr,O) :- atom(O), object(P:Pr,O).
qualified_p_for_uo(P:Pr,U,O) :-
	qualified_p_for_u(P:Pr,U), qualified_p_for_o(P:Pr,O).

qualified_pc_for_u(P:Pr,PC,U) :- atom(U), is_contained_u_in_pc(P:Pr,U,PC).
qualified_pc_for_o(P:Pr,PC,O) :- atom(O), is_contained_o_in_pc(P:Pr,O,PC).
qualified_pc_for_uo(P:Pr,PC,U,O) :-
	qualified_pc_for_u(P:Pr,PC,U), qualified_pc_for_o(P:Pr,PC,O).
%
%
policy_dp1(P:Pr,(_U,_AR,O),PC) :-
	\+ is_contained_o_in_pc(P:Pr, O, PC), !, fail.
policy_dp1(P:Pr,(U,AR,O),_PC) :- atom(P),
	policy(P,Pr),
	associate(P:Pr,UA,ARs,OA),
	member(AR,ARs),
	is_contained_in_ua(P:Pr,U,UA),
	is_contained_in_oa(P:Pr,O,OA).
%
policy_dp(P:Pr,(U,AR,O)) :- atom(P),
	policy(P,Pr),
	associate(P:Pr,UA,ARs,OA),
	member(AR,ARs),
	is_contained_in_ua(P:Pr,U,UA),
	is_contained_in_oa(P:Pr,O,OA).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all derived privileges of the policy
policy_dps(P,DPS) :- atom(P), !, policy(P,Pr),
	findall(DP, policy_dpc(P:Pr,DP), DPs),
	sort(DPs,DPS).
policy_dps(P:Pr,DPS) :-
	findall(DP, policy_dpc(P:Pr,DP), DPs),
	sort(DPs,DPS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_los_objects(P, U, LOSobjs) - objects accessible to user in P
user_los_objects(P, U, LOSobjs) :- atom(P), atom(U),
	policy_dps(P,DPS),
	findall((U,AR,O), member((U,AR,O),DPS), UDPS),
	u_ars_o_view(UDPS,LOSobjs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
u_ars_o_view([],[]).
u_ars_o_view([X|Xs],[(U,ARs,O)|LOS]) :-
	X = (U,_,O),
	findall(AR, member((U,AR,O),[X|Xs]), ARs),
	findall((U,Y,O), member((U,Y,O),Xs), DXs),
	ord_subtract(Xs,DXs,NXs),
	u_ars_o_view(NXs, LOS),
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% los(LOS) - the Logical Object System under a policy
los(P, LOS) :- atom(P), policy(P,Pr),
	findall(user(U), (element(P:Pr,user(U)),assign(P:Pr,U,_)), Users),
	findall(object(O), (object(P:Pr,O),assign(P:Pr,O,_)), Objects),
	findall(user_attribute(UA),
		(element(P:Pr,user_attribute(UA)),assign(P:Pr,_,UA)), UAs1), sort(UAs1,UAs),
	findall(object_attribute(OA),
		(element(P:Pr,object_attribute(OA)),assign(P:Pr,_,OA)), OAs1), sort(OAs1,OAs),
	findall(policy_class(PC), (element(P:Pr,policy_class(PC)),assign(P:Pr,_,PC)), PCs1), sort(PCs1,PCs),
	findall(assign(X,XA), assign(P:Pr,X,XA), Assignments),
	findall(associate(UA,Rights,OA), associate(P:Pr,UA,Rights,OA), Associations),
	element(P:Pr,connector(C)),
	append([Users,Objects,UAs,OAs,PCs,Assignments,Associations,[connector(C)]],LOS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_los(+Policy, +User, -LOS_nodes, -LOS_edges)
user_los(P, U, LOSv, LOSe):- atom(P), policy(P,Pr),
	user_los_v(P:Pr,U,LOSv), user_los_e(P:Pr,LOSv,LOSe).

user_los_v(P, U, LOSv) :- % collect the nodes of the user LOS
	% gc(connector_name,CONN),
        CONN = 'PM',
	findall(PC, element(P,policy_class(PC)), PCs),
	findall(OA, (object_oattribute(P,OA), user_los_oa(P,U,OA)), OAs),
	append([[CONN],PCs,OAs], LOSv).

user_los_oa(P, U, OA) :- % OA is an object attribute of U in P
	element(P,policy_class(PC1)),
	is_contained_plus(P,OA,PC1),
	forall( element(P,policy_class(PC)),
		(\+ is_contained_plus(P,OA,PC)
		;   % user is authorised
		    element(P,user_attribute(UA)),
		    object_oattribute(P,OAm),
		    is_contained_plus(P,U,UA),
		    associate(P,UA,_OPS,OAm),
		    is_contained_plus(P,UA,PC),
		    is_contained_plus(P,OAm,PC),
		    is_contained_star(P,OA,OAm)
		) ), !.

user_los_e(P,LOSv,LOSe) :- % construct the edges of the user LOS
	%gc(connector_name,CONN),
        CONN = 'PM',
	findall(PC, (element(P,policy_class(PC)),member(PC,LOSv)), LOSpcs),
	findall(OA, (object_oattribute(P,OA),member(OA,LOSv)), LOSoas),
	findall((PC,CONN), member(PC,LOSpcs), PC_CONNs),
	findall((OA,PC),
		(   member(OA,LOSoas), member(PC,LOSpcs),
		    is_contained_plus(P,OA,PC),
		    \+ (member(OAp,LOSoas),
			is_contained_plus(P,OA,OAp),
			is_contained_plus(P,OAp,PC))
		), OA_PCs),
	findall((OA1,OA2),
		(   member(OA1,LOSoas), member(OA2,LOSoas),
		    is_contained_plus(P,OA1,OA2),
		    \+ (member(OAp,LOSoas),
			is_contained_plus(P,OA1,OAp),
			is_contained_plus(P,OAp,OA2))
		), OA_OAs),
	append([PC_CONNs,OA_PCs,OA_OAs], LOSe).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_cap(P, U, Cap) - Cap is a current session capability of user in P
user_cap(P, U, (Op, O)) :-
	policy_dps(P,DPS),
	member((U, Op, O), DPS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
access_check(P, (S,M,O)) :-
	atom(P), atom(S), atom(M), atom(O),
	(   sessions:is_session(S,U) ; U = S  ), !, atom(U),
	access_check1(P,(U,M,O)).

access_check1(all, (U,M,O)) :- !,
	param:all_composition(V),
	policy_dpc(all:V, (U,M,O)).
access_check1(P, (U,M,O)) :-
	policy(P,PC),
	policy_dpc(P:PC,(U,M,O)), !.

% extended to deal with combined policies (policies with multiple PCs)
%access_checkc(P, (U,M,O)) :-
%	atom(P), atom(U), atom(M), atom(O),
%	policy(P,PC),
%	policy_dpc(P:PC,(U,M,O)), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compose_policies(P1N,P2N,P3N) :-
	policies:policy(P1N, P1R, P1G),
	policies:policy(P2N, P2R, P2G),
	concat_atom([P1R,'+',P2R],P3R),
	% TODO add the new P3R as a policy class of the new policy
	sort(P1G,P1Gs), sort(P2G,P2Gs), merge_set(P1Gs,P2Gs,P3G),
	retractall(policies:policy(P3N,_,_)),
	assertz(policies:policy(P3N,P3R,P3G)),
	unpack_policy(policy(P3N,P3R,P3G)),
	true.

access(P,U,M,O) :-
	is_contained_in_ua(P,U,UA),
	associate(P,UA,Xs,OA),
	is_contained_in_oa(P,O,OA),
	memberchk(M,Xs),
	true.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% aoa(+P, +U, +PC, -AOA)
aoa(P, U, PC, AOA) :-
	atom(P), policy(P,PC), atom(U), element(P:PC,user(U)),
	atom(PC), element(P:PC,policy_class(PC)),
	findall(OA, aoa1(P, U, PC, OA), AOAu),
	findall(OA1, (member(OA2,AOAu),is_contained_in_oa(P:PC,OA1,OA2)), AOAv),
	append(AOAu,AOAv,AOAuv),
	sort(AOAuv,AOA).

aoa1(P, U, PC, OA) :-
	element(P:PC,user_attribute(UA)),
	is_contained_in_ua(P:PC, U, UA),
	associate(P:PC, UA, _Ops, OA),
	object_oattribute(P:PC,OA),
	is_contained_in(P:PC, UA, PC),
	is_contained_in(P:PC, OA, PC).

min_aoa(P, U, PC, MinAOA) :-
	aoa(P, U, PC, AOA),
	findall(OAm,
		(   member(OAm, AOA),
		    select(OAm, AOA, Aoa),
		    partition(is_contained_plus(P:PC,OAm),Aoa,[],_)
		), MinAOAu),
	sort(MinAOAu,MinAOA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% oa is visible by u in PC
is_visible(P, OA, U, PC) :-
	atom(P), policy(P,_), atom(OA), object_oattribute(P,OA),
	atom(U), element(P,user(U)), atom(PC), element(P,policy_class(PC)),
	min_aoa(P, U, PC, MinAOA),
	member(OAm,MinAOA), is_contained_star(P, OA, OAm),
	findall(PCp, (element(P,policy_class(PCp)),PCp\==PC), PCps),
	is_vis_chk(P, OA, U, PCps).

is_vis_chk(_, _, _, []).
is_vis_chk(P, OA, U, [PCp|PCps]) :-
	\+ is_contained_plus(P, OA, PCp),
	is_vis_chk(P, OA, U, PCps).
is_vis_chk(P, OA, U, [PCp|PCps]) :-
	min_aoa(P, U, PCp, MinAOAp),
	is_vis_chk2(P, OA, MinAOAp),
	is_vis_chk(P, OA, U, PCps).

is_vis_chk2(_,_,[]) :- !, fail.
is_vis_chk2(P, OA,[OAmp|_OAmps]) :-
	is_contained_in(P, OA, OAmp), !.
is_vis_chk2(P, OA,[_,OAmps]) :-
	is_vis_chk2(P, OA, OAmps).

