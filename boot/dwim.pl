/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module('$dwim',
	[ dwim_predicate/2,
	  '$dwim_correct_goal'/3,
	  '$find_predicate'/2,
	  '$similar_module'/2
	]).

:- meta_predicate
	dwim_predicate(:, -),
	'$dwim_correct_goal'(:, +, -),
	'$similar_module'(:, -),
	'$find_predicate'(:, -).

%%	'$dwim_correct_goal'(:Goal, +Bindings, -Corrected)
%
%	Correct a goal (normally typed by the   user)  in the `Do What I
%	Mean' sense. Ask the user to confirm  if a unique correction can
%	be found. Otherwise warn that the   predicate does not exist and
%	fail.

'$dwim_correct_goal'(M:Goal, Bindings, Corrected) :-
	correct_goal(Goal, M, Bindings, Corrected).

correct_goal(Goal, M, _, M:Goal) :-		% Not instantiated. Hope it
	var(Goal), !.				% will be in time
correct_goal((A,B), M, Bindings, (NA,NB)) :- !,
	'$dwim_correct_goal'(M:A, Bindings, NA),
	'$dwim_correct_goal'(M:B, Bindings, NB).
correct_goal((A->B), M, Bindings, (NA->NB)) :- !,
	'$dwim_correct_goal'(M:A, Bindings, NA),
	'$dwim_correct_goal'(M:B, Bindings, NB).
correct_goal((A*->B), M, Bindings, (NA*->NB)) :- !,
	'$dwim_correct_goal'(M:A, Bindings, NA),
	'$dwim_correct_goal'(M:B, Bindings, NB).
correct_goal((A;B), M, Bindings, (NA;NB)) :- !,
	'$dwim_correct_goal'(M:A, Bindings, NA),
	'$dwim_correct_goal'(M:B, Bindings, NB).
correct_goal(\+(A), M, Bindings, \+(NA)) :- !,
	'$dwim_correct_goal'(M:A, Bindings, NA).
correct_goal(Module:Goal, _, _, Module:Goal) :-
	(   var(Module)
	;   var(Goal)
	;   current_predicate(_, Module:Goal)
	), !.
correct_goal(Goal, M, _, Goal) :-		% is defined
	current_predicate(_, M:Goal), !.
correct_goal(Goal, M, Bindings, NewGoal) :-	% correct the goal
	dwim_predicate_list(M:Goal, DWIMs0), !,
	'$module'(TypeIn, TypeIn),
	principal_predicates(TypeIn, DWIMs0, DWIMs),
	correct_literal(M:Goal, Bindings, DWIMs, NewGoal).
correct_goal(Goal, Module, _, NewGoal) :-	% try to autoload
	functor(Goal, Name, Arity),
	'$undefined_procedure'(Module, Name, Arity, Action),
	(   Action == error
	->  existence_error(Module:Name/Arity)
	;   Action == retry
	->  NewGoal = Goal
	;   NewGoal = fail
	).

existence_error(PredSpec) :- !,
	'$module'(TypeIn, TypeIn),
	unqualify_if_context(TypeIn, PredSpec, Spec),
	throw(error(existence_error(procedure, Spec),
		    context(toplevel, 'DWIM could not correct goal'))).

%%	correct_literal(:Goal, +Bindings, +DWIMs, -Corrected) is semidet.
% 
%	Correct a single literal.  DWIMs is a list of heads that can
%	replace the head in Goal.

correct_literal(Goal, Bindings, [Dwim], DwimGoal) :-
	strip_module(Goal, _, G1), 
	strip_module(Dwim, DM, G2), 
	functor(G1, _, Arity), 
	functor(G2, Name, Arity), !,	% same arity: we can replace arguments
	G1 =.. [_|Arguments], 
	G2 =.. [Name|Arguments], 
	'$module'(TypeIn, TypeIn),
	(   '$prefix_module'(DM, TypeIn, G2, DwimGoal),
	    goal_name(DwimGoal, Bindings, String),
	    '$confirm'(dwim_correct(String))
	->  true
	;   DwimGoal = Goal
	).
correct_literal(Goal, Bindings, Dwims, NewGoal) :-
	strip_module(Goal, _, G1), 
	functor(G1, _, Arity), 
	include_arity(Dwims, Arity, [Dwim]), !,
	correct_literal(Goal, Bindings, [Dwim], NewGoal).
correct_literal(Goal, _, Dwims, _) :-
	print_message(error, dwim_undefined(Goal, Dwims)),
	fail.

include_arity([], _, []).
include_arity([H|T0], Arity, [H|T]) :-
	strip_module(H, _, G), 
	functor(G, _, Arity), !,
	include_arity(T0, Arity, T).
include_arity([_|T0], Arity, T)	:-
	include_arity(T0, Arity, T).


%	goal_name(+Goal, +Bindings, -Name)
%	
%	Transform Goal into a readable format by binding its variables.

goal_name(Goal, Bindings, String) :-
	State = s(_),
	(   bind_vars(Bindings),
	    numbervars(Goal, 0, _, [singletons(true), attvar(skip)]),
	    format(string(S), '~q', [Goal]),
	    nb_setarg(1, State, S),
	    fail
	;   arg(1, State, String)
	).

bind_vars([]).
bind_vars([Name=Var|T]) :-
	Var = '$VAR'(Name),		% portray prints Name
	bind_vars(T).


%%	'$find_predicate'(:Spec, -List) is det.
%
%	Unify `List' with a list  of  predicate  heads  that  match  the
%	specification  `Spec'.  `Spec' is a term Name/Arity, a ``Head'', 
%	or just an atom.  The latter refers to  all  predicate  of  that
%	name with arbitrary arity.  `Do What I Mean' correction is done.
%	If the requested module is `user' predicates residing in any
%	module will be considered matching.
%	
%	@error	existence_error(procedure, Spec) if no matching predicate
%		can be found.

'$find_predicate'(M:S, List) :-
	name_arity(S, Name, Arity),
	'$module'(TypeIn, TypeIn),
	(   M == user
	->  true
	;   Module = M
	),
	find_predicate(Module, TypeIn, Name, Arity, L0), !,
	sort(L0, L1),
	principal_predicates(TypeIn, L1, List).
'$find_predicate'(_:S, List) :-
	name_arity(S, Name, Arity),
	findall(Head, ('$in_library'(Name, Arity, _Path),
		       functor(Head, Name, Arity)), List),
	List \== [], !.
'$find_predicate'(Spec, _) :-
	existence_error(Spec).
	
find_predicate(Module, C, Name, Arity, VList) :-
	findall(Head, find_predicate_(Module, C, Name, Arity, Head), VList),
	VList \== [], !.
find_predicate(Module, C, Name, Arity, Pack) :-
	findall(Head, find_sim_pred(Module, Name, Arity, Head), List),
	pack(List, Module, Arity, C, Packs),
	'$member'(Dwim-Pack, Packs),
	unqualify_if_context(C, Dwim, PredName),
	'$confirm'(dwim_correct(PredName)), !.

unqualify_if_context(_, X, X) :-
	var(X), !.
unqualify_if_context(C, C2:X, X) :-
	C == C2, !.
unqualify_if_context(_, X, X) :- !.

%%	pack(+Heads, +Context, -Packs)
%	
%	Pack the list of heads into packets, consisting of the corrected
%	specification and a list of heads satisfying this specification.

pack([], _, _, _, []) :- !.
pack([M:T|Rest], Module, Arity, C, [Name-[H|R]|Packs]) :-
	'$prefix_module'(M, C, T, H),
	pack_name(M:T, Module, Arity, Name),
	pack_(Module, Arity, Name, C, Rest, R, NewRest),
	pack(NewRest, Module, Arity, C, Packs).

pack_(Module, Arity, Name, C, List, [H|R], Rest) :-
	'$select'(M:T, List, R0),
	pack_name(M:T, Module, Arity, Name), !,
	'$prefix_module'(M, C, T, H),
	pack_(Module, Arity, Name, C, R0, R, Rest).
pack_(_, _, _, _, Rest, [], Rest).

pack_name(_:T, V1, V2,   Name)   :- var(V1), var(V2), !, functor(T, Name, _).
pack_name(M:T,  _, V2, M:Name)   :-          var(V2), !, functor(T, Name, _).
pack_name(_:T, V1,  _, Name/A)   :- var(V1),          !, functor(T, Name, A).
pack_name(M:T,  _,  _, M:Name/A) :-                      functor(T, Name, A).


find_predicate_(Module, C, Name, Arity, Head) :-
	current_module(Module),
	current_predicate(Name, Module:Term),
	functor(Term, Name, Arity),
	'$prefix_module'(Module, C, Term, Head).

find_sim_pred(M, Name, Arity, Module:Term) :-
	sim_module(M, Module),
	'$dwim_predicate'(Module:Name, Term),
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

%%	name_arity(+Spec, -Name, -Arity)
%	
%	Obtain the name and arity of a predicate specification. Warn if
%	this is not a legal specification.

name_arity(Atom, Atom, _) :-
	atom(Atom), !.
name_arity(Name/Arity, Name, Arity) :- !.
name_arity(Name//DCGArity, Name, Arity) :-
	(   var(DCGArity)
	->  true
	;   Arity is DCGArity+2
	).
name_arity(Term, Name, Arity) :-
	callable(Term), !,
	functor(Term, Name, Arity).
name_arity(Spec, _, _) :-
	throw(error(type_error(predicate_indicator, Spec), _)).


%%	principal_predicates(+Context, +Heads, -Principals)
%	
%	Get the principal predicate list from a list of heads (e.g., the
%	module in which the predicate is defined).

principal_predicates(C, Heads, Principals) :-
	find_definitions(Heads, C, P0),
	(   C == user
	->  find_publics(P0, P1),
	    delete_defaults(P1, P1, P2)
	;   P2 = P0
	),	
	'$list_to_set'(P2, Principals).		% remove duplicates
	
delete_defaults([], _, []) :- !.
delete_defaults([system:Head|T], L, R) :-
	memberchk(Head, L), !,
	delete_defaults(T, L, R).
delete_defaults([H|T], L, [H|R]) :-
	delete_defaults(T, L, R).

find_publics([], []).
find_publics([H0|T0], [H|T]) :-
	find_public(H0, H),
	find_publics(T0, T).

find_public(Head, user:Term) :-
	strip_module(Head, M, Term),
	current_predicate(_, user:Term),
	'$predicate_property'(imported_from(M), user:Term), !.
find_public(Head, Head).

find_definitions([], _, []).
find_definitions([H0|T0], M, [H|T]) :-
	find_definition(M, H0, H),
	find_definitions(T0, M, T).

find_definition(C, Head, Principal) :-
	'$predicate_property'(imported_from(Module), C:Head), !,
	strip_module(Head, _, Term),
	'$prefix_module'(Module, C, Term, P0),
	find_definition(C, P0, Principal).
find_definition(_, Head, Head).


%%	dwim_predicate(:Head, -NewHead) is nondet.
%	
%	Find a head that is in a `Do What I Mean' sence the same as `Head'.
%	backtracking produces more such predicates.

dwim_predicate(Head, DWIM) :-
	dwim_predicate_list(Head, DWIMs),
	'$member'(DWIM, DWIMs).

dwim_predicate_list(Head, [Head]) :-
	current_predicate(_, Head), !.
dwim_predicate_list(M:Head, DWIMs) :-
	setof(DWIM, dwim_pred(M:Head, DWIM), DWIMs), !.
dwim_predicate_list(Head, DWIMs) :-
	setof(DWIM, '$similar_module'(Head, DWIM), DWIMs), !.
dwim_predicate_list(_:Goal, DWIMs) :-
	setof(Module:Goal,
	      ( current_module(Module),
		current_predicate(_, Module:Goal)
	      ), DWIMs).

dwim_pred(Head, Dwim) :-
	strip_module(Head, Module, H),
	default_module(Module, M),
	'$dwim_predicate'(M:H, Dwim).

%%	'$similar_module'(:Goal, -DWIMGoal) is nondet.
%
%	True if DWIMGoal exists and is, except from a typo in the
%	module specification, equivalent to Goal.

'$similar_module'(Module:Goal, DwimModule:Goal) :-
	current_module(DwimModule),
	dwim_match(Module, DwimModule),
	current_predicate(_, DwimModule:Goal).
