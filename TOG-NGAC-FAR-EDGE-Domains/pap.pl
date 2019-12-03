% Policy Access/Administration Point

:- module(pap, [permitted_add_delete_policy_elements/1,
		add_policy_element/2, add_policy_element/3,
		delete_policy_element/2, delete_policy_element/3,
		add_policy_elements/2, add_policy_elements/3,
		delete_policy_elements/2, delete_policy_elements/3,
	        compose_policies/3,
	        get_current_policy/1, set_current_policy/1,
	        get_current_gpolicy/1, set_current_gpolicy/1,
	        load_policy/2, load_policy_immediate/2, unload_policy/1]
	 ).

:- use_module(param).
:- use_module(dpl).
:- use_module(sessions).
:- use_module(audit,[audit_gen/2]). % currently not used in this module

permitted_add_delete_policy_elements([user,object,assign,associate]).

%
% Policy Administration Point commands
% called externally through paapi
% or internally through the pap exported predicates
%
% IN THE FOLLOWING NEED TO CHECK FOR EFFECTS OF CONDITIONAL RULES

% add_policy_element/2
add_policy_element(P:PC,Element) :- !, atom(P), atom(PC), add_policy_element(P,PC,Element).
add_policy_element(P,Element) :- atom(P), policy(P,PC), add_policy_element(P,PC,Element).

% add_policy_element/3
add_policy_element(P,PC,user(U)) :- \+element(P:PC,user(U)),	!, passert( element(P:PC,user(U)) ).
add_policy_element(P,PC,object(O)) :- \+element(P:PC,object(O)), !, passert( element(P:PC,object(O)) ).
add_policy_element(P,PC,assign(E,Attr)) :-
	( ( element(P:PC,user(E)), element(P:PC,user_attribute(Attr)) ) % must be user to user_attribute
	;
	  ( element(P:PC,object(E)), element(P:PC,object_attribute(Attr)) ) % or object to object_attribute
	),
	\+assign(P:PC,E,Attr), % must be no current assignment
	!,
	passert( assign(P:PC,E,Attr) ).
add_policy_element(P,PC,associate(A,R,B)) :- atom(A), atom(B), ground(R), is_list(R),
	element(P:PC,user_attribute(A)), element(P:PC,object_attribute(B)),
	\+associate(P:PC,A,R,B),
	!,
	passert( associate(P:PC,A,R,B) ).


add_policy_elements(P:PC,Elements) :- !, atom(P), atom(PC), add_policy_elements(P,PC,Elements).
add_policy_elements(P,Elements) :- atom(P), policy(P,PC), add_policy_elements(P,PC,Elements).

add_policy_elements(_,_,[]).
add_policy_elements(P,PC,[Element|Elements]) :-
	add_policy_element(P,PC,Element),
	add_policy_elements(P,PC,Elements).


% delete_policy_element/2
delete_policy_element(P:PC,Element) :- !, atom(P), atom(PC), delete_policy_element(P,PC,Element).
delete_policy_element(P,Element) :- atom(P), policy(P,PC), delete_policy_element(P,PC,Element).

% delete_policy_element/3
delete_policy_element(P,PC,user(U)) :- element(P:PC,user(U)),
	% there must be no current assignment of the user
	\+assign(P:PC,U,_), !,	pretract( element(P:PC,user(U)) ).
delete_policy_element(P,PC,object(O)) :- element(P:PC,object(O)),
	% there must be no current assignment of the object
	\+assign(P:PC,O,_), !,	pretract( element(P:PC,object(O)) ).
delete_policy_element(P,PC,assign(E,Attr)) :-  assign(P:PC,E,Attr), !, pretract( assign(P:PC,E,Attr) ).
delete_policy_element(P,PC,associate(A,R,B)) :- atom(A), atom(B), ground(R), is_list(R),
	associate(P:PC,A,R,B),
	!,
	pretract( associate(P:PC,A,R,B) ).


delete_policy_elements(P:PC,Elements) :- !, atom(P), atom(PC), delete_policy_elements(P,PC,Elements).
delete_policy_elements(P,Elements) :- atom(P), policy(P,PC), delete_policy_elements(P,PC,Elements).

delete_policy_elements(_,_,[]).
delete_policy_elements(P,PC,[Element|Elements]) :-
	delete_policy_element(P,PC,Element),
	delete_policy_elements(P,PC,Elements).

passert(PI) :-	%format('asserting ~q~n',[PI]),
	assert(dpl:PI).
pretract(PI) :- %format('retracting ~q~n',[PI]),
	retractall(dpl:PI).

compose_policies(P1N,P2N,P3N) :-
	policies:policy(P1N, P1R, P1G),
	policies:policy(P2N, P2R, P2G),
	concat_atom([P1R,'+',P2R],P3R),
	% TODO add the new P3R as a policy class of the new policy
	sort(P1G,P1Gs), sort(P2G,P2Gs), merge_set(P1Gs,P2Gs,P3G),
	retractall(policies:policy(P3N,_,_)),
	assertz(policies:policy(P3N,P3R,P3G)),
	dpl:unpack_policy(policy(P3N,P3R,P3G)),
	true.

get_current_policy(P) :- param:current_policy(P).

set_current_policy(Pname) :- atom(Pname), !,
	(   Pname == all
	->  dpl:clear_policy, Name=Pname
	;   ( Pname == allnc % no clear - used only for testing
	    ->  Name=all
	    ;	Name=Pname
	    )
	),
	% should check that the policy is defined
	param:setparam(current_policy,Name).

get_current_gpolicy(GP) :- param:current_gpolicy(GP).

set_current_gpolicy(GPname) :- atom(GPname), !,
	% should check that the policy is defined
	param:setparam(current_gpolicy,GPname).

load_policy(Pfile,PolicyName) :-
	dpl:load_decl_policy(Pfile,PolicyName).

load_policy_immediate(Pspec,PolicyName) :-
	dpl:load_decl_policy_immediate(Pspec,PolicyName).

unload_policy(P) :-
	dpl:purge_policy(P),
	(   param:current_policy(P)
	->  param:setparam(current_policy,none)
	;   true
	).
