/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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
$dwim_correct_goal(Goal, _, NewGoal) :-
	$strip_module(Goal, Module, G1),
	functor(G1, Name, Arity),
	$undefined_procedure(Module, Name, Arity, Action),
	(   Action == error
	->  existence_error(Module:Name/Arity)
	;   Action == retry
	->  NewGoal = Goal
	;   NewGoal = fail
	).

existence_error(user:Name/Arity) :- !,
	throw(error(existence_error(procedure, Name/Arity), _)).
existence_error(Module:Name/Arity) :- !,
	throw(error(existence_error(procedure, Module:Name/Arity), _)).

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
	$confirm(dwim_correct(String)).
correct_goal(Goal, Bindings, Dwims, NewGoal) :-
	$strip_module(Goal, _, G1), 
	functor(G1, _, Arity), 
	sublist($dwim:has_arity(Arity), Dwims, [Dwim]), !,
	correct_goal(Goal, Bindings, [Dwim], NewGoal).
correct_goal(Goal, _, Dwims, _) :-
	tag_module(Goal, MGoal),
	tag_modules(Dwims, MDwims),
	print_message(error, dwim_undefined(MGoal, MDwims)),
	fail.

:- module_transparent
	tag_module/2,
	tag_modules/2.

tag_module(T, M:T2) :-
	$strip_module(T, M, T2).

tag_modules([], []).
tag_modules([H0|T0], [H|T]) :-
	tag_module(H0, H),
	tag_modules(T0, T).

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
	$member(Dwim-Pack, Packs),
	print_pack_name(C, Dwim, PredName),
	$confirm(dwim_correct(PredName)), !.

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
	$select(M:T, List, R0),
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
	throw(error(type_error(predicate_indicator, Spec), _)).


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
	sort(P2, Principals).		% remove duplicates
	
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
	$member(DWIM, DWIMs).

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
