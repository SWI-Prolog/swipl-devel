/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2015, VU University Amsterdam

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
	  [ safe_goal/1,		% :Goal
	    safe_call/1			% :Goal
	  ]).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(prolog_format)).
:- use_module(library(apply)).

:- multifile
	safe_primitive/1,		% Goal
	safe_meta_predicate/1,		% Name/Arity
	safe_meta/2,			% Goal, Calls
	safe_global_variable/1,		% Name
	safe_directive/1.		% Module:Goal

% :- debug(sandbox).

/** <module> Sandboxed Prolog code

Prolog is a full-featured Turing complete  programming language in which
it is easy to write programs that can   harm your computer. On the other
hand, Prolog is a logic based _query language_ which can be exploited to
query data interactively from, e.g.,  the   web.  This  library provides
safe_goal/1, which determines whether it is safe to call its argument.

@tbd	Handling of ^ and // meta predicates
@tbd	Complete set of whitelisted predicates
@see	http://www.swi-prolog.org/pldoc/package/pengines.html
*/


:- meta_predicate
	safe_goal(:),
	safe_call(0).

%%	safe_call(:Goal)
%
%	Call Goal if it  complies  with   the  sandboxing  rules. Before
%	calling   Goal,   it   performs   expand_goal/2,   followed   by
%	safe_goal/1. Expanding is done explicitly  because situations in
%	which safe_call/1 typically concern goals that  are not known at
%	compile time.
%
%	@see safe_goal/1.

safe_call(Goal0) :-
	expand_goal(Goal0, Goal),
	safe_goal(Goal),
	call(Goal).

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
%	  - It is not allowed to make explicitly qualified calls into
%	  modules to predicates that are not exported or declared
%	  public.
%
%	@error	instantiation_error if the analysis encounters a term in
%		a callable position that is insufficiently instantiated
%		to determine the predicate called.
%	@error	permission_error(call, sandboxed, Goal) if Goal is in
%		the call-tree and not white-listed.

:- thread_local
	last_error/1.

safe_goal(M:Goal) :-
	empty_assoc(Safe0),
	catch(safe(Goal, M, [], Safe0, _), E, true), !,
	retractall(last_error(_)),
	(   var(E)
	->  true
	;   throw(E)
	).
safe_goal(_) :-
	last_error(E), !,
	retractall(last_error(_)),
	throw(E).
safe_goal(G) :-
	debug(sandbox(fail), 'safe_goal/1 failed for ~p', [G]),
	throw(error(instantiation_error, sandbox(G, []))).


%%	safe(+Goal, +Module, +Parents, +Safe0, -Safe) is semidet.
%
%	Is true if Goal can only call safe code.

safe(V, _, Parents, _, _) :-
	var(V), !,
	Error = error(instantiation_error, sandbox(V, Parents)),
	asserta(last_error(Error)),
	throw(Error).
safe(M:G, _, Parents, Safe0, Safe) :- !,
	must_be(atom, M),
	must_be(callable, G),
	(   predicate_property(M:G, imported_from(M2))
	->  true
	;   M2 = M
	),
	(   (   safe_primitive(M2:G)
	    ;   safe_primitive(G),
		predicate_property(G, iso)
	    )
	->  Safe = Safe0
	;   (   predicate_property(M:G, exported)
	    ;	predicate_property(M:G, public)
	    ;	predicate_property(M:G, multifile)
	    ;	predicate_property(M:G, iso)
	    ;	memberchk(M:_, Parents)
	    )
	->  safe(G, M, Parents, Safe0, Safe)
	;   throw(error(permission_error(call, sandboxed, M:G),
			sandbox(M:G, Parents)))
	).
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
	(   safe_primitive(M2:G)
	;   predicate_property(M2:G, number_of_rules(0))
	), !.
safe(G, M, Parents, Safe0, Safe) :-
	predicate_property(G, iso),
	safe_meta_call(G, Called), !,
	safe_list(Called, M, Parents, Safe0, Safe).
safe(G, M, Parents, Safe0, Safe) :-
	(   predicate_property(M:G, imported_from(M2))
	->  true
	;   M2 = M
	),
	safe_meta_call(M2:G, Called), !,
	safe_list(Called, M, Parents, Safe0, Safe).
safe(G, M, Parents, Safe0, Safe) :-
	goal_id(M:G, Id, Gen),
	(   get_assoc(Id, Safe0, _)
	->  Safe = Safe0
	;   put_assoc(Id, Safe0, true, Safe1),
	    (	Gen == M:G
	    ->	safe_clauses(Gen, M, [Id|Parents], Safe1, Safe)
	    ;	catch(safe_clauses(Gen, M, [Id|Parents], Safe1, Safe),
		      error(instantiation_error, _),
		      fail)
	    )
	), !.
safe(G, M, Parents, _, _) :-
	debug(sandbox(fail),
	      'safe/1 failed for ~p (parents:~p)', [M:G, Parents]),
	fail.

safe_clauses(G, M, Parents, Safe0, Safe) :-
	predicate_property(M:G, interpreted), !,
	def_module(M:G, MD:QG),
	findall(Ref-Body, clause(MD:QG, Body, Ref), Bodies),
	safe_bodies(Bodies, MD, Parents, Safe0, Safe).
safe_clauses(G, M, [_|Parents], _, _) :-
	predicate_property(M:G, visible), !,
	throw(error(permission_error(call, sandboxed, G),
		    sandbox(M:G, Parents))).
safe_clauses(_, _, [G|Parents], _, _) :-
	throw(error(existence_error(procedure, G),
		    sandbox(G, Parents))).

%%	safe_bodies(+Bodies, +Module, +Parents, +Safe0, -Safe)
%
%	Verify the safety of bodies. If  a   clause  was compiled with a
%	qualified module, we  consider  execution  of   the  body  in  a
%	different module _not_ a cross-module call.

safe_bodies([], _, _, Safe, Safe).
safe_bodies([Ref-H|T], M, Parents, Safe0, Safe) :-
	(   H = M2:H2, nonvar(M2),
	    clause_property(Ref, module(M2))
	->  copy_term(H2, H3),
	    CM = M2
	;   copy_term(H, H3),
	    CM = M
	),
	safe(H3, CM, Parents, Safe0, Safe1),
	safe_bodies(T, M, Parents, Safe1, Safe).

def_module(M:G, MD:QG) :-
	predicate_property(M:G, imported_from(MD)), !,
	meta_qualify(MD:G, M, QG).
def_module(M:G, M:QG) :-
	meta_qualify(M:G, M, QG).

%%	safe_list(+Called, +Module, +Parents, +Safe0, -Safe)
%
%	Processed objects called through meta  predicates. If the called
%	object  is  in  our  current  context    we  remove  the  module
%	qualification to avoid the cross-module check.

safe_list([], _, _, Safe, Safe).
safe_list([H|T], M, Parents, Safe0, Safe) :-
	(   H = M2:H2,
	    M == M2				% in our context
	->  copy_term(H2, H3)
	;   copy_term(H, H3)			% cross-module call
	),
	safe(H3, M, Parents, Safe0, Safe1),
	safe_list(T, M, Parents, Safe1, Safe).

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
q_arg(^).
q_arg(//).

qualify(A, M, MZ:Q) :-
	strip_module(M:A, MZ, Q).

%%	goal_id(:Goal, -Id, -Gen) is nondet.
%
%	Generate an identifier for the goal proven to be safe. We
%	first try to prove the most general form of the goal.  If
%	this fails, we try to prove more specific versions.
%
%	@tbd	Do step-by-step generalisation instead of the current
%		two levels (most general and most specific).
%	@tbd	We could also use variant_sha1 for the goal ids.

goal_id(M:Goal, M:Id, Gen) :- !,
	goal_id(Goal, Id, Gen).
goal_id(Var, _, _) :-
	var(Var), !,
	instantiation_error(Var).
goal_id(Atom, Atom, Atom) :-
	atom(Atom), !.
goal_id(Term, _, _) :-
	\+ compound(Term), !,
	type_error(callable, Term).
goal_id(Term, Skolem, Gen) :-		% most general form
	compound_name_arity(Term, Name, Arity),
	compound_name_arity(Skolem, Name, Arity),
	compound_name_arity(Gen, Name, Arity),
	copy_goal_args(1, Term, Skolem, Gen),
	(   Gen =@= Term
	->  !				% No more specific one; we can commit
	;   true
	),
	numbervars(Skolem, 0, _).
goal_id(Term, Skolem, Term) :-		% most specific form
	debug(sandbox(specify), 'Retrying with ~p', [Term]),
	copy_term(Term, Skolem),
	numbervars(Skolem, 0, _).

%%	copy_goal_args(+I, +Term, +Skolem, +Gen) is det.
%
%	Create  the  most  general  form,   but  keep  module  qualified
%	arguments because they will likely be called anyway.

copy_goal_args(I, Term, Skolem, Gen) :-
	arg(I, Term, TA), !,
	arg(I, Skolem, SA),
	arg(I, Gen, GA),
	copy_goal_arg(TA, SA, GA),
	I2 is I + 1,
	copy_goal_args(I2, Term, Skolem, Gen).
copy_goal_args(_, _, _, _).

copy_goal_arg(Arg, SArg, Arg) :-
	copy_goal_arg(Arg), !,
	copy_term(Arg, SArg).
copy_goal_arg(_, _, _).

copy_goal_arg(Var) :- var(Var), !, fail.
copy_goal_arg(_:_).

dcg_goal(X) :-
	var(X), !, fail.
dcg_goal(phrase(_,_,_)).
dcg_goal(dcg_call(_,_,_)).

%%	verify_safe_declaration(+Decl)
%
%	See whether a  safe  declaration  makes   sense.  That  is,  the
%	predicate must be defined (such that  the attacker cannot define
%	the predicate), must be sufficiently   instantiated and only ISO
%	declared predicates may omit a module qualification.
%
%	@tbd	Verify safe_meta/2 declarations.  It is a bit less clear
%		what the rules are.

term_expansion(safe_primitive(Goal), Term) :-
	(   verify_safe_declaration(Goal)
	->  Term = safe_primitive(Goal)
	;   Term = []
	).

system:term_expansion(sandbox:safe_primitive(Goal), Term) :-
	(   verify_safe_declaration(Goal)
	->  Term = sandbox:safe_primitive(Goal)
	;   Term = []
	).

verify_safe_declaration(Var) :-
	var(Var), !,
	instantiation_error(Var).
verify_safe_declaration(Module:Goal) :-
	must_be(atom, Module),
	must_be(callable, Goal),
	(   ok_meta(Module:Goal)
	->  true
	;   (   predicate_property(Module:Goal, visible)
	    ->	true
	    ;	predicate_property(Module:Goal, foreign)
	    ),
	    \+ predicate_property(Module:Goal, imported_from(_)),
	    \+ predicate_property(Module:Goal, meta_predicate(_))
	->  true
	;   permission_error(declare, safe_goal, Module:Goal)
	).
verify_safe_declaration(Goal) :-
	must_be(callable, Goal),
	(   predicate_property(system:Goal, iso),
	    \+ predicate_property(system:Goal, meta_predicate())
	->  true
	;   permission_error(declare, safe_goal, Goal)
	).

ok_meta(system:assert(_)).
ok_meta(system:use_module(_,_)).
ok_meta(system:use_module(_)).

verify_predefined_safe_declarations :-
	forall(clause(safe_primitive(Goal), _Body, Ref),
	       ( catch(verify_safe_declaration(Goal), E, true),
		 (   nonvar(E)
		 ->  clause_property(Ref, file(File)),
		     clause_property(Ref, line_count(Line)),
		     print_message(error, bad_safe_declaration(Goal, File, Line))
		 ;   true
		 )
	       )).

:- initialization(verify_predefined_safe_declarations, now).

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
%	predicate is imported from the given module.

% First, all ISO system predicates that are considered safe

safe_primitive(true).
safe_primitive(fail).
safe_primitive(system:false).
safe_primitive(repeat).
safe_primitive(!).
					% types
safe_primitive(var(_)).
safe_primitive(nonvar(_)).
safe_primitive(system:attvar(_)).
safe_primitive(integer(_)).
safe_primitive(float(_)).
safe_primitive(system:rational(_)).
safe_primitive(number(_)).
safe_primitive(atom(_)).
safe_primitive(system:blob(_,_)).
safe_primitive(system:string(_)).
safe_primitive(atomic(_)).
safe_primitive(compound(_)).
safe_primitive(callable(_)).
safe_primitive(ground(_)).
safe_primitive(system:cyclic_term(_)).
safe_primitive(acyclic_term(_)).
safe_primitive(system:is_stream(_)).
					% ordering
safe_primitive(@>(_,_)).
safe_primitive(@>=(_,_)).
safe_primitive(==(_,_)).
safe_primitive(@<(_,_)).
safe_primitive(@=<(_,_)).
safe_primitive(compare(_,_,_)).
safe_primitive(sort(_,_)).
safe_primitive(keysort(_,_)).
safe_primitive(system: =@=(_,_)).
safe_primitive(system:'$btree_find_node'(_,_,_,_)).

					% unification and equivalence
safe_primitive(=(_,_)).
safe_primitive(\=(_,_)).
safe_primitive(system:'?='(_,_)).
safe_primitive(system:unifiable(_,_,_)).
safe_primitive(unify_with_occurs_check(_,_)).
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
safe_primitive(system:nb_setarg(_,_,_)).
safe_primitive(functor(_,_,_)).
safe_primitive(_ =.. _).
safe_primitive(system:compound_name_arity(_,_,_)).
safe_primitive(system:compound_name_arguments(_,_,_)).
safe_primitive(copy_term(_,_)).
safe_primitive(system:duplicate_term(_,_)).
safe_primitive(system:copy_term_nat(_,_)).
safe_primitive(numbervars(_,_,_)).
safe_primitive(subsumes_term(_,_)).
safe_primitive(system:term_hash(_,_)).
safe_primitive(system:term_hash(_,_,_,_)).
safe_primitive(system:variant_sha1(_,_)).
safe_primitive(system:'$term_size'(_,_,_)).

					% dicts
safe_primitive(system:is_dict(_)).
safe_primitive(system:is_dict(_,_)).
safe_primitive(system:get_dict(_,_,_)).
safe_primitive(system:get_dict(_,_,_,_,_)).
safe_primitive(system:'$get_dict_ex'(_,_,_)).
safe_primitive(system:dict_create(_,_,_)).
safe_primitive(system:dict_pairs(_,_,_)).
safe_primitive(system:put_dict(_,_,_)).
safe_primitive(system:put_dict(_,_,_,_)).
safe_primitive(system:del_dict(_,_,_,_)).
safe_primitive(system:select_dict(_,_,_)).
safe_primitive(system:b_set_dict(_,_,_)).
safe_primitive(system:nb_set_dict(_,_,_)).
safe_primitive(system:nb_link_dict(_,_,_)).
safe_primitive(system:(:<(_,_))).
safe_primitive(system:(>:<(_,_))).
					% atoms
safe_primitive(atom_chars(_, _)).
safe_primitive(atom_codes(_, _)).
safe_primitive(sub_atom(_,_,_,_,_)).
safe_primitive(atom_concat(_,_,_)).
safe_primitive(atom_length(_,_)).
safe_primitive(system:atomic_list_concat(_,_,_)).
safe_primitive(system:downcase_atom(_,_)).
safe_primitive(system:upcase_atom(_,_)).
safe_primitive(system:char_type(_,_)).
safe_primitive(system:normalize_space(_,_)).
safe_primitive(system:sub_atom_icasechk(_,_,_)).
					% numbers
safe_primitive(number_codes(_,_)).
safe_primitive(number_chars(_,_)).
safe_primitive(system:atom_number(_,_)).
safe_primitive(system:code_type(_,_)).
					% strings
safe_primitive(system:atom_string(_,_)).
safe_primitive(system:number_string(_,_)).
safe_primitive(system:string_chars(_, _)).
safe_primitive(system:string_codes(_, _)).
safe_primitive(system:string_code(_,_,_)).
safe_primitive(system:sub_string(_,_,_,_,_)).
safe_primitive(system:split_string(_,_,_,_)).
safe_primitive(system:atomics_to_string(_,_,_)).
safe_primitive(system:atomics_to_string(_,_)).
safe_primitive(system:string_concat(_,_,_)).
safe_primitive(system:string_length(_,_)).
safe_primitive(system:string_lower(_,_)).
safe_primitive(system:string_upper(_,_)).
safe_primitive(system:term_string(_,_)).
					% Lists
safe_primitive(length(_,_)).
					% exceptions
safe_primitive(throw(_)).
safe_primitive(system:abort).
					% misc
safe_primitive(current_prolog_flag(_,_)).
safe_primitive(current_op(_,_,_)).
safe_primitive(system:sleep(_)).
safe_primitive(system:thread_self(_)).
safe_primitive(system:get_time(_)).
safe_primitive(system:statistics(_,_)).
safe_primitive(system:thread_statistics(Id,_,_)) :-
	(   var(Id)
	->  instantiation_error(Id)
	;   thread_self(Id)
	).
safe_primitive(system:thread_property(Id,_)) :-
	(   var(Id)
	->  instantiation_error(Id)
	;   thread_self(Id)
	).
safe_primitive(system:format_time(_,_,_)).
safe_primitive(system:format_time(_,_,_,_)).
safe_primitive(system:date_time_stamp(_,_)).
safe_primitive(system:stamp_date_time(_,_,_)).
safe_primitive(system:strip_module(_,_,_)).
safe_primitive('$messages':message_to_string(_,_)).
safe_primitive(system:import_module(_,_)).
safe_primitive(system:file_base_name(_,_)).
safe_primitive(system:file_directory_name(_,_)).
safe_primitive(system:file_name_extension(_,_,_)).

safe_primitive(clause(H,_)) :- safe_clause(H).
safe_primitive(asserta(X)) :- safe_assert(X).
safe_primitive(assertz(X)) :- safe_assert(X).
safe_primitive(retract(X)) :- safe_assert(X).
safe_primitive(retractall(X)) :- safe_assert(X).

% We need to do data flow analysis to find the tag of the
% target key before we can conclude that functions on dicts
% are safe.
safe_primitive('$dicts':'.'(_,K,_)) :- atom(K).
safe_primitive('$dicts':'.'(_,K,_)) :-
	nonvar(K),
	dict_built_in(K).

dict_built_in(get(_)).
dict_built_in(put(_)).
dict_built_in(put(_,_)).

% The non-ISO system predicates.  These can be redefined, so we must
% be careful to ensure the system ones are used.

safe_primitive(system:false).
safe_primitive(system:cyclic_term(_)).
safe_primitive(system:msort(_,_)).
safe_primitive(system:sort(_,_,_,_)).
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
safe_primitive(system:get_attrs(_,_)).
safe_primitive(system:term_attvars(_,_)).
safe_primitive(system:del_attr(_,_)).
safe_primitive(system:del_attrs(_)).
safe_primitive('$attvar':copy_term(_,_,_)).
					% globals
safe_primitive(system:b_getval(_,_)).
safe_primitive(system:b_setval(Var,_)) :-
	safe_global_var(Var).
safe_primitive(system:nb_getval(_,_)).
safe_primitive('$syspreds':nb_setval(Var,_)) :-
	safe_global_var(Var).
safe_primitive(system:nb_current(_,_)).
					% database
safe_primitive(system:assert(X)) :-
	safe_assert(X).
					% Output
safe_primitive(system:writeln(_)).
safe_primitive('$messages':print_message(_,_)).

% use_module/1.  We only allow for .pl files that are loaded from
% relative paths that do not contain /../

safe_primitive(system:use_module(Spec, _Import)) :-
	safe_primitive(system:use_module(Spec)).
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

%%	safe_clause(+Head) is semidet.
%
%	Consider a call to clause safe if  it   does  not try to cross a
%	module boundary. Cross-module usage  of   clause/2  can  extract
%	private information from other modules.

safe_clause(H) :- var(H), !.
safe_clause(_:_) :- !, fail.
safe_clause(_).


%%	safe_global_var(+Name) is semidet.
%
%	True if Name  is  a  global   variable  to  which  assertion  is
%	considered safe.

safe_global_var(Name) :-
	var(Name), !,
	instantiation_error(Name).
safe_global_var(Name) :-
	safe_global_variable(Name).

%%	safe_global_variable(Name) is semidet.
%
%	Declare the given global variable safe to write to.


%%	safe_meta(+Goal, -Called:list(callable)) is semidet.
%
%	Hook. True if Goal is a   meta-predicate that is considered safe
%	iff all elements in Called are safe.

safe_meta(system:put_attr(V,M,A), Called) :- !,
	(   atom(M)
	->  attr_hook_predicates([ attr_unify_hook(A, _),
				   attribute_goals(V,_,_),
				   project_attributes(_,_)
				 ], M, Called)
	;   instantiation_error(M)
	).
safe_meta(system:with_output_to(Output, G), [G]) :-
	safe_output(Output), !.
safe_meta(system:format(Format, Args), Calls) :-
	format_calls(Format, Args, Calls).
safe_meta(system:format(Output, Format, Args), Calls) :-
	safe_output(Output),
	format_calls(Format, Args, Calls).
safe_meta(prolog_debug:debug(_Term, Format, Args), Calls) :-
	format_calls(Format, Args, Calls).
safe_meta('$attvar':freeze(_Var,Goal), [Goal]).
safe_meta(phrase(NT,Xs0,Xs), [Goal]) :-	% phrase/2,3 and call_dcg/2,3
	expand_nt(NT,Xs0,Xs,Goal).
safe_meta(phrase(NT,Xs0), [Goal]) :-
	expand_nt(NT,Xs0,[],Goal).
safe_meta('$dcg':call_dcg(NT,Xs0,Xs), [Goal]) :-
	expand_nt(NT,Xs0,Xs,Goal).
safe_meta('$dcg':call_dcg(NT,Xs0), [Goal]) :-
	expand_nt(NT,Xs0,[],Goal).

%%	attr_hook_predicates(+Hooks0, +Module, -Hooks) is det.
%
%	Filter the defined hook implementations.   This  is safe because
%	(1) calling an undefined predicate is   not  a safety issue, (2)
%	the  user  an  only  assert  in  the  current  module  and  only
%	predicates that have a safe body. This avoids the need to define
%	attribute hooks solely for the purpose of making them safe.

attr_hook_predicates([], _, []).
attr_hook_predicates([H|T], M, Called) :-
	(   predicate_property(M:H, interpreted)
	->  Called = [M:H|Rest]
	;   Called = Rest
	),
	attr_hook_predicates(T, M, Rest).


%%	expand_nt(+NT, ?Xs0, ?Xs, -NewGoal)
%
%	Similar to expand_phrase/2, but we do   throw  errors instead of
%	failing if NT is not sufficiently instantiated.

expand_nt(NT, _Xs0, _Xs, _NewGoal) :-
	strip_module(NT, _, Plain),
	var(Plain), !,
	instantiation_error(Plain).
expand_nt(NT, Xs0, Xs, NewGoal) :-
	dcg_translate_rule((pseudo_nt --> NT),
			   (pseudo_nt(Xs0c,Xsc) :- NewGoal0)),
	(   var(Xsc), Xsc \== Xs0c
	->  Xs = Xsc, NewGoal1 = NewGoal0
	;   NewGoal1 = (NewGoal0, Xsc = Xs)
	),
	(   var(Xs0c)
	->  Xs0 = Xs0c,
	    NewGoal = NewGoal1
	;   NewGoal = ( Xs0 = Xs0c, NewGoal1 )
	).

%%	safe_meta_call(+Goal, -Called:list(callable)) is semidet.
%
%	True if Goal is a   meta-predicate that is considered safe
%	iff all elements in Called are safe.

safe_meta_call(Goal, _Called) :-
	debug(sandbox(meta), 'Safe meta ~p?', [Goal]),
	fail.
safe_meta_call(Goal, Called) :-
	safe_meta(Goal, Called), !.	% call hook
safe_meta_call(Goal, Called) :-
	Goal = M:Plain,
	compound(Plain),
	compound_name_arity(Plain, Name, Arity),
	safe_meta_predicate(M:Name/Arity),
	predicate_property(Goal, meta_predicate(Spec)), !,
	findall(C, called(Spec, Plain, C), Called).
safe_meta_call(M:Goal, Called) :- !,
	generic_goal(Goal, Gen),
	safe_meta(M:Gen),
	findall(C, called(Gen, Goal, C), Called).
safe_meta_call(Goal, Called) :-
	generic_goal(Goal, Gen),
	safe_meta(Gen),
	findall(C, called(Gen, Goal, C), Called).

called(Gen, Goal, Called) :-
	arg(I, Gen, Spec),
	calling_meta_spec(Spec),
	arg(I, Goal, Called0),
	extend(Spec, Called0, Called).

generic_goal(G, Gen) :-
	functor(G, Name, Arity),
	functor(Gen, Name, Arity).

calling_meta_spec(V) :- var(V), !, fail.
calling_meta_spec(I) :- integer(I), !.
calling_meta_spec(^).
calling_meta_spec(//).


extend(^, G, Plain) :- !,
	strip_existential(G, Plain).
extend(//, DCG, Goal) :- !,
	(   expand_phrase(call_dcg(DCG,_,_), Goal)
	->  true
	;   instantiation_error(DCG)	% Ask more instantiation.
	).				% might not help, but does not harm.
extend(0, G, G) :- !.
extend(I, M:G0, M:G) :- !,
	G0 =.. List,
	length(Extra, I),
	append(List, Extra, All),
	G =.. All.
extend(I, G0, G) :-
	G0 =.. List,
	length(Extra, I),
	append(List, Extra, All),
	G =.. All.

strip_existential(Var, Var) :-
	var(Var), !.
strip_existential(M:G0, M:G) :- !,
	strip_existential(G0, G).
strip_existential(_^G0, G) :- !,
	strip_existential(G0, G).
strip_existential(G, G).

%%	safe_meta(?Template).

safe_meta((0,0)).
safe_meta((0;0)).
safe_meta((0->0)).
safe_meta((0*->0)).
safe_meta(catch(0,*,0)).
safe_meta(findall(*,0,*)).
safe_meta('$bags':findall(*,0,*,*)).
safe_meta(setof(*,^,*)).
safe_meta(bagof(*,^,*)).
safe_meta('$bags':findnsols(*,*,0,*)).
safe_meta('$bags':findnsols(*,*,0,*,*)).
safe_meta(system:call_cleanup(0,0)).
safe_meta(system:setup_call_cleanup(0,0,0)).
safe_meta(system:setup_call_catcher_cleanup(0,0,*,0)).
safe_meta('$attvar':call_residue_vars(0,*)).
safe_meta(system:call_with_inference_limit(0,*,*)).
safe_meta(system:call_with_depth_limit(0,*,*)).
safe_meta(^(*,0)).
safe_meta(\+(0)).
safe_meta(call(0)).
safe_meta(call(1,*)).
safe_meta(call(2,*,*)).
safe_meta(call(3,*,*,*)).
safe_meta(call(4,*,*,*,*)).
safe_meta(call(5,*,*,*,*,*)).
safe_meta(call(6,*,*,*,*,*,*)).


%%	safe_output(+Output)
%
%	True if something is a safe output argument for with_output_to/2
%	and friends. We do not want writing to streams.

safe_output(Output) :-
	var(Output), !,
	instantiation_error(Output).
safe_output(atom(_)).
safe_output(string(_)).
safe_output(codes(_)).
safe_output(codes(_,_)).
safe_output(chars(_)).
safe_output(chars(_,_)).
safe_output(current_output).
safe_output(current_error).

%%	format_calls(+Format, +FormatArgs, -Calls)
%
%	Find ~@ calls from Format and Args.

:- public format_calls/3.			% used in pengines_io

format_calls(Format, _Args, _Calls) :-
	var(Format), !,
	instantiation_error(Format).
format_calls(Format, Args, Calls) :-
	format_types(Format, Types),
	(   format_callables(Types, Args, Calls)
	->  true
	;   throw(error(format_error(Format, Types, Args), _))
	).

format_callables([], [], []).
format_callables([callable|TT], [G|TA], [G|TG]) :- !,
	format_callables(TT, TA, TG).
format_callables([_|TT], [_|TA], TG) :- !,
	format_callables(TT, TA, TG).


		 /*******************************
		 *    SAFE COMPILATION HOOKS	*
		 *******************************/

:- multifile
	prolog:sandbox_allowed_directive/1,
	prolog:sandbox_allowed_expansion/1.

%%	prolog:sandbox_allowed_directive(:G) is det.
%
%	Throws an exception if G is not considered a safe directive.

prolog:sandbox_allowed_directive(Directive) :-
	debug(sandbox(directive), 'Directive: ~p', [Directive]),
	fail.
prolog:sandbox_allowed_directive(Directive) :-
	safe_directive(Directive), !.
prolog:sandbox_allowed_directive(M:PredAttr) :-
	\+ prolog_load_context(module, M), !,
	debug(sandbox(directive), 'Cross-module directive', []),
	permission_error(directive, sandboxed, (:- M:PredAttr)).
prolog:sandbox_allowed_directive(M:PredAttr) :-
	safe_pattr(PredAttr), !,
	PredAttr =.. [Attr, Preds],
	(   safe_pattr(Preds, Attr)
	->  true
	;   permission_error(directive, sandboxed, (:- M:PredAttr))
	).
prolog:sandbox_allowed_directive(_:Directive) :-
	safe_source_directive(Directive), !.
prolog:sandbox_allowed_directive(_:Directive) :-
	directive_loads_file(Directive, File), !,
	safe_path(File).
prolog:sandbox_allowed_directive(G) :-
	safe_goal(G).

%%	safe_directive(:Directive) is semidet.
%
%	Hook to declare additional directives as safe. The argument is a
%	term `Module:Directive` (without =|:-|= wrapper).  In almost all
%	cases, the implementation must verify that   the `Module` is the
%	current load context as illustrated  below.   This  check is not
%	performed by the system to  allow   for  cases  where particular
%	cross-module directives are allowed.
%
%	  ==
%	  sandbox:safe_directive(M:Directive) :-
%	      prolog_load_context(module, M),
%	      ...
%	  ==


safe_pattr(dynamic(_)).
safe_pattr(thread_local(_)).
safe_pattr(volatile(_)).
safe_pattr(discontiguous(_)).
safe_pattr(public(_)).
safe_pattr(meta_predicate(_)).

safe_pattr(Var, _) :-
	var(Var), !,
	instantiation_error(Var).
safe_pattr((A,B), Attr) :- !,
	safe_pattr(A, Attr),
	safe_pattr(B, Attr).
safe_pattr(M:G, Attr) :- !,
	(   atom(M),
	    prolog_load_context(module, M)
	->  true
	;   Goal =.. [Attr,M:G],
	    permission_error(directive, sandboxed, (:- Goal))
	).
safe_pattr(_, _).

safe_source_directive(op(_,_,Name)) :- !,
	(   atom(Name)
	->  true
	;   is_list(Name),
	    maplist(atom, Name)
	).
safe_source_directive(set_prolog_flag(Flag, Value)) :- !,
	atom(Flag), ground(Value),
	safe_directive_flag(Flag, Value).
safe_source_directive(style_check(_)).

directive_loads_file(use_module(library(X)), X).
directive_loads_file(use_module(library(X), _Imports), X).
directive_loads_file(ensure_loaded(library(X)), X).
directive_loads_file(include(X), X).

safe_path(X) :-
	var(X), !,
	instantiation_error(X).
safe_path(X) :-
	(   atom(X)
	;   string(X)
	), !,
	\+ sub_atom(X, 0, _, 0, '..'),
	\+ sub_atom(X, 0, _, _, '/'),
	\+ sub_atom(X, 0, _, _, '../'),
	\+ sub_atom(X, _, _, 0, '/..'),
	\+ sub_atom(X, _, _, _, '/../').
safe_path(A/B) :- !,
	safe_path(A),
	safe_path(B).


%%	safe_directive_flag(+Flag, +Value) is det.
%
%	True if it is safe to set the flag Flag in a directive to Value.
%
%	@tbd	If we can avoid that files are loaded after changing
%		this flag, we can allow for more flags.

safe_directive_flag(generate_debug_info, _).

%%	prolog:sandbox_allowed_expansion(:G) is det.
%
%	Throws an exception if G  is   not  considered  a safe expansion
%	goal. This deals with call-backs from the compiler for
%
%	  - goal_expansion/2
%	  - term_expansion/2
%	  - Quasi quotations.
%
%	Our assumption is that external expansion rules are coded safely
%	and we only need to be  careful   if  the sandboxed code defines
%	expansion rules.

prolog:sandbox_allowed_expansion(Directive) :-
	prolog_load_context(module, M),
	debug(sandbox(expansion), 'Expand in ~p: ~p', [M, Directive]),
	fail.
prolog:sandbox_allowed_expansion(M:G) :-
	prolog_load_context(module, M), !,
	safe_goal(M:G).
prolog:sandbox_allowed_expansion(_,_).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1,
	prolog:message_context//1,
	prolog:error_message//1.

prolog:message_context(sandbox(_G, [])) --> !.
prolog:message_context(sandbox(_G, Parents)) -->
	[ nl, 'Reachable from:'-[] ],
	callers(Parents, 10).

callers([], _) --> !.
callers(_,  0) --> !.
callers([G|Parents], Level) -->
	{ NextLevel is Level-1
	},
	[ nl, '	  ~p'-[G] ],
	callers(Parents, NextLevel).

prolog:message(bad_safe_declaration(Goal, File, Line)) -->
	[ '~w:~d: Invalid safe_primitive/1 declaration: ~p'-
	  [File, Line, Goal] ].

prolog:error_message(format_error(Format, Types, Args)) -->
	format_error(Format, Types, Args).

format_error(Format, Types, Args) -->
	{ length(Types, TypeLen),
	  length(Args, ArgsLen),
	  (   TypeLen > ArgsLen
	  ->  Problem = 'not enough'
	  ;   Problem = 'too many'
	  )
	},
	[ 'format(~q): ~w arguments (found ~w, need ~w)'-
	  [Format, Problem, ArgsLen, TypeLen]
	].
