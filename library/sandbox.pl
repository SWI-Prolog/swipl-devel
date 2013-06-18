/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2013, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sandbox,
	  [ safe_goal/1			% :Goal
	  ]).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply_macros), [expand_phrase/2]).

:- multifile
	safe_primitive/1,		% Goal
	safe_meta/2.			% Goal, Calls

% :- debug(sandbox).

/** <module> Sandboxed Prolog code

Prolog is a full-featured Turing complete  programming language in which
it is easy to write programs that can   harm your computer. On the other
hand, Prolog is a logic based _query language_ which can be exploited to
query data interactively from, e.g.,  the   web.  This  library provides
safe_goal/1, which determines whether it is safe to call its argument.

@tbd	Handling of ^ and // meta predicates
*/


:- meta_predicate
	safe_goal(0).

%%	safe_goal(:Goal) is det.
%
%	True if calling Goal provides  no   security  risc. This implies
%	that:
%
%	  - The call-graph can be fully expanded. Full expansion *stops*
%	  if a meta-goal is found for   which we cannot determine enough
%	  details to know which predicate will be called.
%
%	  - All predicates  referenced  from   the  fully  expanded  are
%	  whitelisted by the predicate safe_primitive/1 and safe_meta/2.
%
%	@error	instantiation_error if the analysis encounters a term in
%		a callable position that is insufficiently instantiated
%		to determine the predicate called.
%	@error	permission_error(call, sandboxed, Goal) if Goal is in
%		the call-tree and not white-listed.

safe_goal(M:Goal) :-
	empty_assoc(Safe0),
	safe(Goal, M, [], Safe0, _).


%%	safe(+Goal, +Module, +Parents, +Safe0, -Safe) is semidet.
%
%	Is true if Goal can only call safe code.

safe(V, _, Parents, _, _) :-
	var(V), !,
	throw(error(instantiation_error, sandbox(V, Parents))).
safe(M:G, _, Parent, Safe0, Safe) :- !,
	safe(G, M, Parent, Safe0, Safe).
safe(G, _, Parents, _, _) :-
	debugging(sandbox(show)),
	length(Parents, Level),
	debug(sandbox(show), '[~D] SAFE ~q?', [Level, G]),
	fail.
safe(G, _, _, Safe, Safe) :-
	safe_primitive(G),
	predicate_property(G, iso), !.
safe(G, M, _, Safe, Safe) :-
	(   predicate_property(M:G, imported_from(M2))
	->  true
	;   M2 = M
	),
	safe_primitive(M2:G), !.
safe(G, M, Parents, Safe0, Safe) :-
	safe_meta(G, Called), !,
	safe_list(Called, M, Parents, Safe0, Safe).
safe(G, M, Parents, Safe0, Safe) :-
	goal_id(M:G, Id, Gen),
	(   get_assoc(Id, Safe0, _)
	->  Safe = Safe0
	;   put_assoc(Id, Safe0, true, Safe1),
	    safe_clauses(Gen, M, [Id|Parents], Safe1, Safe)
	).

safe_clauses(G, M, Parents, Safe0, Safe) :-
	predicate_property(M:G, interpreted), !,
%	\+ predicate_property(M:G, meta_predicate(_)), !,
	def_module(M:G, MD:QG),
	findall(Body, clause(MD:QG, Body), Bodies),
	safe_list(Bodies, MD, Parents, Safe0, Safe).
safe_clauses(_, _M, [G|Parents], _, _) :-
	throw(error(permission_error(call, sandboxed, G),
		    sandbox(G, Parents))).

safe_list([], _, _, Safe, Safe).
safe_list([H|T], M, Parents, Safe0, Safe) :-
	copy_term(H, H1),
	safe(H1, M, Parents, Safe0, Safe1),
	safe_list(T, M, Parents, Safe1, Safe).


def_module(M:G, MD:QG) :-
	predicate_property(M:G, imported_from(MD)), !,
	meta_qualify(MD:G, M, QG).
def_module(M:G, M:QG) :-
	meta_qualify(M:G, M, QG).

%%	meta_qualify(:G, +M, -QG) is det.
%
%	Perform meta-qualification of the goal-argument

meta_qualify(MD:G, M, QG) :-
	predicate_property(MD:G, meta_predicate(Head)), !,
	G =.. [Name|Args],
	Head =.. [_|Q],
	qualify_args(Q, M, Args, QArgs),
	QG =.. [Name|QArgs].
meta_qualify(_:G, _, G).

qualify_args([], _, [], []).
qualify_args([H|T], M, [A|AT], [Q|QT]) :-
	qualify_arg(H, M, A, Q),
	qualify_args(T, M, AT, QT).

qualify_arg(S, M, A, Q) :-
	q_arg(S), !,
	qualify(A, M, Q).
qualify_arg(_, _, A, A).

q_arg(I) :- integer(I), !.
q_arg(:).

qualify(A, M, MZ:Q) :-
	strip_module(M:A, MZ, Q).

%%	goal_id(:Goal, -Id, -Gen) is nondet.
%
%	Generate an identifier for the goal proven to be safe. We
%	first try to prove the most general form of the goal.  If
%	this fails, we try to prove more specific versions.
%
%	@tbd Do step-by-step generalisation instead of the current
%	two levels (most general and most specific).
%

goal_id(M:Goal, M:Id, Gen) :- !,
	goal_id(Goal, Id, Gen).
goal_id(Term, _, _) :-
	\+ callable(Term), !, fail.
goal_id(Term, Name/Arity, Gen) :-	% most general form
	functor(Term, Name, Arity),
	functor(Gen, Name, Arity).
goal_id(Term, Skolem, Term) :-		% most specific form
	copy_term(Term, Skolem),
	numbervars(Skolem, 0, _).

%%	safe_primitive(?Goal) is nondet.
%
%	True if Goal is safe  to   call  (i.e.,  cannot access dangerous
%	system-resources and cannot upset  other   parts  of  the Prolog
%	process). There are two  types  of   facts.  ISO  built-ins  are
%	declared without a module prefix. This is safe because it is not
%	allowed to (re-)define these  primitives   (i.e.,  give  them an
%	unsafe     implementation)     and     the       way      around
%	(redefine_system_predicate/1) is unsafe.  The   other  group are
%	module-qualified and only match if the   system  infers that the
%	predicate is (or will be) imported from the given module.

% First, all ISO system predicates that are considered safe

safe_primitive(true).
safe_primitive(fail).
safe_primitive(repeat).
safe_primitive(!).
					% types
safe_primitive(var(_)).
safe_primitive(nonvar(_)).
safe_primitive(integer(_)).
safe_primitive(float(_)).
safe_primitive(atom(_)).
safe_primitive(compound(_)).
safe_primitive(ground(_)).
					% ordering
safe_primitive(@>(_,_)).
safe_primitive(@>=(_,_)).
safe_primitive(==(_,_)).
safe_primitive(@<(_,_)).
safe_primitive(@=<(_,_)).
safe_primitive(compare(_,_,_)).
safe_primitive(sort(_,_)).
safe_primitive(keysort(_,_)).
					% unification and equivalence
safe_primitive(=(_,_)).
safe_primitive(\==(_,_)).
					% arithmetic
safe_primitive(is(_,_)).
safe_primitive(>(_,_)).
safe_primitive(>=(_,_)).
safe_primitive(=:=(_,_)).
safe_primitive(=\=(_,_)).
safe_primitive(=<(_,_)).
safe_primitive(<(_,_)).
					% term-handling
safe_primitive(arg(_,_,_)).
safe_primitive(system:setarg(_,_,_)).
safe_primitive(functor(_,_,_)).
safe_primitive(_ =.. _).
safe_primitive(copy_term(_,_)).
safe_primitive(numbervars(_,_,_)).
					% atoms
safe_primitive(atom_concat(_,_,_)).
safe_primitive(atom_chars(_, _)).
					% Lists
safe_primitive(length(_,_)).
					% exceptions
safe_primitive(throw(_)).
					% misc
safe_primitive(current_prolog_flag(_,_)).

safe_primitive(clause(_,_)).
safe_primitive(asserta(X)) :- safe_assert(X).
safe_primitive(assertz(X)) :- safe_assert(X).
safe_primitive(retract(X)) :- safe_assert(X).
safe_primitive(retractall(X)) :- safe_assert(X).

% The non-ISO system predicates.  These can be redefined, so we must
% be careful to ensure the system ones are used.

safe_primitive(system:false).
safe_primitive(system:cyclic_term(_)).
safe_primitive(system:msort(_,_)).
safe_primitive(system:between(_,_,_)).
safe_primitive(system:succ(_,_)).
safe_primitive(system:plus(_,_,_)).
safe_primitive(system:term_variables(_,_)).
safe_primitive(system:atom_to_term(_,_,_)).
safe_primitive(system:term_to_atom(_,_)).
safe_primitive(system:atomic_list_concat(_,_,_)).
safe_primitive(system:atomic_list_concat(_,_)).
safe_primitive(system:downcase_atom(_,_)).
safe_primitive(system:upcase_atom(_,_)).
safe_primitive(system:is_list(_)).
safe_primitive(system:memberchk(_,_)).
safe_primitive(system:'$skip_list'(_,_,_)).
					% attributes
safe_primitive(system:get_attr(_,_,_)).
safe_primitive(system:del_attr(_,_)).
					% globals
safe_primitive(system:b_getval(_,_)).
safe_primitive(system:b_setval(_,_)).
safe_primitive(system:nb_current(_,_)).
safe_primitive(system:assert(X)) :-
	safe_assert(X).

% use_module/1.  We only allow for .pl files that are loaded from
% relative paths that do not contain /../

safe_primitive(system:use_module(Spec)) :-
	ground(Spec),
	(   atom(Spec)
	->  Path = Spec
	;   Spec =.. [_Alias, Segments],
	    phrase(segments_to_path(Segments), List),
	    atomic_list_concat(List, Path)
	),
	\+ is_absolute_file_name(Path),
	\+ sub_atom(Path, _, _, _, '/../'),
	absolute_file_name(Spec, AbsFile,
			   [ access(read),
			     file_type(prolog),
			     file_errors(fail)
			   ]),
	file_name_extension(_, Ext, AbsFile),
	save_extension(Ext).

% Other library predicates.

					% rdf
safe_primitive(rdf_db:rdf(_,_,_)).
safe_primitive(rdf_db:rdf(_,_,_,_)).
					% http
safe_primitive(http_session:http_session_data(_)).
safe_primitive(http_session:http_session_id(_)).
					% random
safe_primitive(random:random(_)).
					% porter
safe_primitive(porter_stem:porter_stem(_,_)).
safe_primitive(porter_stem:unaccent_atom(_,_)).
safe_primitive(porter_stem:tokenize_atom(_,_)).
safe_primitive(porter_stem:atom_to_stem_list(_,_)).

% support predicates for safe_primitive, validating the safety of
% arguments to certain goals.

segments_to_path(A/B) --> !,
	segments_to_path(A),
	[/],
	segments_to_path(B).
segments_to_path(X) -->
	[X].

save_extension(pl).

%%	safe_assert(+Term) is semidet.
%
%	True if assert(Term) is safe,  which   means  it  asserts in the
%	current module. Cross-module asserts are   considered unsafe. We
%	only allow for adding facts. In theory,  we could also allow for
%	rules if we prove the safety of the body.

safe_assert(C) :- cyclic_term(C), !, fail.
safe_assert(X) :- var(X), !, fail.
safe_assert(_Head:-_Body) :- !, fail.
safe_assert(_:_) :- !, fail.
safe_assert(_).

%%	safe_meta(+Goal, -Called) is semidet.
%
%	True if Goal is a meta-predicate that is considered safe iff all
%	elements in Called are safe.

safe_meta(put_attr(_,M,A), [M:attr_unify_hook(A, _)]) :-
	atom(M), !.
safe_meta(Phrase, [Goal]) :-
	expand_phrase(Phrase, Goal), !.
safe_meta(Goal, Called) :-
	generic_goal(Goal, Gen),
	safe_meta(Gen),
	findall(C, called(Gen, Goal, C), Called).

called(Gen, Goal, Called) :-
	arg(I, Gen, Spec),
	integer(Spec),
	arg(I, Goal, Called0),
	extend(Spec, Called0, Called).

generic_goal(G, Gen) :-
	functor(G, Name, Arity),
	functor(Gen, Name, Arity).

extend(0, G, G) :- !.
extend(I, G0, G) :-
	G0 =.. List,
	length(Extra, I),
	append(List, Extra, All),
	G =.. All.

safe_meta((0,0)).
safe_meta((0;0)).
safe_meta((0->0)).
safe_meta(forall(0,0)).
safe_meta(catch(0,_,0)).
safe_meta(findall(_,0,_)).
safe_meta(findall(_,0,_,_)).
safe_meta(setof(_,0,_)).		% TBD
safe_meta(bagof(_,0,_)).
safe_meta(^(_,0)).
safe_meta(\+(0)).
safe_meta(maplist(1, _)).
safe_meta(maplist(2, _, _)).
safe_meta(maplist(3, _, _, _)).
safe_meta(call(0)).
safe_meta(call(1, _)).
safe_meta(call(2, _, _)).
safe_meta(call(3, _, _, _)).
safe_meta(call(4, _, _, _, _)).
safe_meta(call(5, _, _, _, _, _)).
