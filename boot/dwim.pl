/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Introduce `Do What I Mean' (DWIM) correction
*/

:- module($dwim,
	[ dwim_predicate/2
	, $dwim_correct_goal/3
	, $find_predicate/2
	, $similar_module/2
	]).

:- module_transparent
	$dwim_correct_goal/3, 
	correct_goal/4.

%	$dwim_correct_goal(+Goal, +Bindings, -Corrected)
%	Correct a goal (normally typed by the user) in the `Do What I Mean'
%	sence. Ask the user to confirm if the a unique correction can be
%	found. Otherwise warn that the predicate does not exist and fail.

$dwim_correct_goal(Goal, _, Goal) :-		% Not instantiated. Hope it
	var(Goal), !.				% will be in time
$dwim_correct_goal((A,B), Bindings, (NA,NB)) :- !,
	$dwim_correct_goal(A, Bindings, NA),
	$dwim_correct_goal(B, Bindings, NB).
$dwim_correct_goal((A->B;C), Bindings, (NA->NB;NC)) :- !,
	$dwim_correct_goal(A, Bindings, NA),
	$dwim_correct_goal(B, Bindings, NB),
	$dwim_correct_goal(C, Bindings, NC).
$dwim_correct_goal((A*->B;C), Bindings, (NA*->NB;NC)) :- !,
	$dwim_correct_goal(A, Bindings, NA),
	$dwim_correct_goal(B, Bindings, NB),
	$dwim_correct_goal(C, Bindings, NC).
$dwim_correct_goal((A;B), Bindings, (NA;NB)) :- !,
	$dwim_correct_goal(A, Bindings, NA),
	$dwim_correct_goal(B, Bindings, NB).
$dwim_correct_goal(\+(A), Bindings, \+(NA)) :- !,
	$dwim_correct_goal(A, Bindings, NA).
$dwim_correct_goal(Module:Goal, _, Module:Goal) :-
	(var(Module) ; var(Goal)), !.
$dwim_correct_goal(Goal, _, Goal) :-		% is defined
	current_predicate(_, Goal), !.
$dwim_correct_goal(Goal, Bindings, NewGoal) :-	% correct the goal
	dwim_predicate_list(Goal, DWIMs0), !,
	context_module(C),
	principal_predicates(C, DWIMs0, DWIMs),
	correct_goal(Goal, Bindings, DWIMs, NewGoal).
$dwim_correct_goal(Goal, _, _) :-		% can't be corrected
	$break($warn_undefined(Goal, [])),
	fail.

correct_goal(Goal, Bindings, [Dwim], DwimGoal) :-
	$strip_module(Goal, _, G1), 
	$strip_module(Dwim, DM, G2), 
	functor(G1, _, Arity), 
	functor(G2, Name, Arity), !, 
	G1 =.. [_|Arguments], 
	G2 =.. [Name|Arguments], 
	context_module(Context),
	$prefix_module(DM, Context, G2, DwimGoal),
	goal_name(DwimGoal, Bindings, String),
	$confirm('Correct to: `~w''', [String]).
correct_goal(Goal, Bindings, Dwims, NewGoal) :-
	$strip_module(Goal, _, G1), 
	functor(G1, _, Arity), 
	sublist($dwim:has_arity(Arity), Dwims, [Dwim]), !,
	correct_goal(Goal, Bindings, [Dwim], NewGoal).
correct_goal(Goal, _, Dwims, _) :-
	$break($warn_undefined(Goal, Dwims)), 
	fail.

has_arity(A, G) :-
	$strip_module(G, _, G1), 
	functor(G1, _, A).

%	goal_name(+Goal, +Bindings, -Name)
%	Transform Goal into a readable format.

goal_name(Goal, Bindings, String) :-
	checklist(call, Bindings),		% Bind the variables
	goal_name_(Goal, String),
	recorda($goal_name, String),
	fail.
goal_name(_, _, String) :-
	recorded($goal_name, String, Ref), !,
	erase(Ref).

goal_name_('_', '_') :- !.			% catch anonymous variable
goal_name_(Module:Name/Arity, String) :- !,
	sformat(String, '~q:~q/~q', [Module, Name, Arity]).
goal_name_(Name/Arity, String) :- !,
	sformat(String, '~q/~q', [Name, Arity]).
goal_name_(Module:Term, String) :- !,
	sformat(String, '~q:~w', [Module, Term]).
goal_name_(Goal, String) :-
	sformat(String, '~w', [Goal]).


%	$find_predicate(+Spec, -List)
%
%	Unify `List' with a list  of  predicate  heads  that  match  the
%	specification  `Spec'.  `Spec' is a term Name/Arity, a ``Head'', 
%	or just an atom.  The latter refers to  all  predicate  of  that
%	name with arbitrary arity.  `Do What I Mean' correction is done.
%	If the requested module is `user' predicates residing in any
%	module will be considered matching.
%	If  no predicates can be found or more than one `Do What I Mean'
%	solution exists an error message is displayed.

:- module_transparent
	$find_predicate/2.

$find_predicate(Spec, List) :-
	$strip_module(Spec, M, S),
	name_arity(S, Name, Arity),
	context_module(C),
	(   M == user
	;   Module = M
	) ->
	find_predicate(Module, C, Name, Arity, L0), !,
	sort(L0, L1),
	principal_predicates(C, L1, List).
$find_predicate(Spec, List) :-
	$strip_module(Spec, _M, S),
	name_arity(S, Name, Arity),
	findall(Head, ('$in_library'(Name, Arity),
		       functor(Head, Name, Arity)), List),
	List \== [], !.
$find_predicate(Spec, _) :-
	print_message(error, no_predicates_for(Spec)),
	fail.
	
find_predicate(Module, C, Name, Arity, VList) :-
	findall(Head, find_predicate_(Module, C, Name, Arity, Head), VList),
	VList \== [], !.
find_predicate(Module, C, Name, Arity, Pack) :-
	findall(Head, find_sim_pred(Module, Name, Arity, Head), List),
	pack(List, Module, Arity, C, Packs),
	member(Dwim-Pack, Packs),
	print_pack_name(C, Dwim, PredName),
	$confirm('Correct to `~w''', PredName), !.

print_pack_name(C, C:Name/Arity, P) :- !, concat_atom([Name, /, Arity], P).
print_pack_name(_, M:Name/Arity, P) :- !, concat_atom([M, :, Name, /, Arity], P).
print_pack_name(C, C:Name, Name)    :- !.
print_pack_name(_, M:Name, P)       :- !, concat_atom([M, :, Name], P).
print_pack_name(_, Name,   Name).


%	pack(+Heads, +Context, -Packs)
%	Pack the list of heads into packets, consisting of the corrected
%	specification and a list of heads satisfying this specification.

pack([], _, _, _, []) :- !.
pack([M:T|Rest], Module, Arity, C, [Name-[H|R]|Packs]) :-
	$prefix_module(M, C, T, H),
	pack_name(M:T, Module, Arity, Name),
	pack_(Module, Arity, Name, C, Rest, R, NewRest),
	pack(NewRest, Module, Arity, C, Packs).

pack_(Module, Arity, Name, C, List, [H|R], Rest) :-
	select(List, M:T, R0),
	pack_name(M:T, Module, Arity, Name), !,
	$prefix_module(M, C, T, H),
	pack_(Module, Arity, Name, C, R0, R, Rest).
pack_(_, _, _, _, Rest, [], Rest).

pack_name(_:T, V1, V2,   Name)   :- var(V1), var(V2), !, functor(T, Name, _).
pack_name(M:T,  _, V2, M:Name)   :-          var(V2), !, functor(T, Name, _).
pack_name(_:T, V1,  _, Name/A)   :- var(V1),          !, functor(T, Name, A).
pack_name(M:T,  _,  _, M:Name/A) :-                      functor(T, Name, A).


find_predicate_(M, C, Name, Arity, Head) :-
	same_module(M, Module),
	current_predicate(Name, Module:Term),
	functor(Term, Name, A),
	same_arity(Arity, A),
	$prefix_module(Module, C, Term, Head).

same_module(M, Module) :-
	var(M), !,
	current_module(Module).
same_module(M, M) :-
	current_module(M).

same_arity(A, _) :- var(A), !.
same_arity(A, A).

find_sim_pred(M, Name, Arity, Module:Term) :-
	sim_module(M, Module),
	$dwim_predicate(Module:Name, Term),
	functor(Term, _, DArity),
	sim_arity(Arity, DArity).
	
sim_module(M, Module) :-
	var(M), !,
	current_module(Module).
sim_module(M, M) :-
	current_module(M), !.
sim_module(M, Module) :-
	current_module(Module),
	dwim_match(M, Module).

sim_arity(A, _) :- var(A), !.
sim_arity(A, D) :- abs(A-D) < 2.

%	name_arity(+Spec, -Name, -Arity)
%	Obtain the name and arity of a predicate specification. Warn if
%	this is not a legal specification.

name_arity(Atom, Atom, _) :-
	atom(Atom), !.
name_arity(Name/Arity, Name, Arity) :- !.
name_arity(Term, Name, Arity) :-
	functor(Term, Name, Arity), !.
name_arity(Spec, _, _) :-
	$warning('Illegal predicate specification: `~w''', [Spec]),
	fail.


%	principal_predicates(+Context, +Heads, -Principals)
%	Get the principal predicate list from a list of heads (e.g. the
%	module in which the predicate is defined).

principal_predicates(C, Heads, Principals) :-
	maplist(find_definition(C), Heads, P0),
	(   C == user
	->  maplist(find_public, P0, P1),
	    delete_defaults(P1, P1, P2)
	;   P2 = P0
	),	
	list_to_set(P2, Principals).
	
delete_defaults([], _, []) :- !.
delete_defaults([system:Head|T], L, R) :-
	memberchk(Head, L), !,
	delete_defaults(T, L, R).
delete_defaults([H|T], L, [H|R]) :-
	delete_defaults(T, L, R).

find_public(Head, user:Term) :-
	$strip_module(Head, M, Term),
	current_predicate(_, user:Term),
	$predicate_property(imported_from(M), user:Term), !.
find_public(Head, Head).

find_definition(C, Head, Principal) :-
	$predicate_property(imported_from(Module), C:Head), !,
	$strip_module(Head, _, Term),
	$prefix_module(Module, C, Term, P0),
	find_definition(C, P0, Principal).
find_definition(_, Head, Head).


%	dwim_predicate(+Head, -NewHead)
%	Find a head that is in a `Do What I Mean' sence the same as `Head'.
%	backtracking produces more such predicates.

:- module_transparent
	dwim_predicate/2, 
	dwim_predicate_list/2.

dwim_predicate(Head, DWIM) :-
	dwim_predicate_list(Head, DWIMs),
	member(DWIM, DWIMs).

dwim_predicate_list(Head, [Head]) :-
	current_predicate(_, Head), !.
dwim_predicate_list(Head, DWIMs) :-
	context_module(C),
	setof(DWIM, $dwim:dwim_pred(C:Head, DWIM), DWIMs), !.
dwim_predicate_list(Head, DWIMs) :-
	setof(DWIM, $similar_module(Head, DWIM), DWIMs), !.
dwim_predicate_list(Head, DWIMs) :-
	$strip_module(Head, _, Goal),
	setof(Module:Goal, ( current_module(Module),
			     current_predicate(_, Module:Goal)
			   ), DWIMs).

dwim_pred(Head, Dwim) :-
	'$strip_module'(Head, Module, H),
	default_module(Module, M),
	'$dwim_predicate'(M:H, Dwim).

$similar_module(Head, DwimModule:Goal) :-
	$strip_module(Head, Module, Goal),
	current_module(DwimModule),
	dwim_match(Module, DwimModule),
	current_predicate(_, DwimModule:Goal).
