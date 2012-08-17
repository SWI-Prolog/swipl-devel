/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

:- module('$expand',
	  [ expand_term/2,
	    expand_goal/2
	  ]).

/** <module> Prolog source-code transformation

This module specifies, together with dcg.pl, the transformation of terms
as they are read from a file before they are processed by the compiler.

The toplevel is expand_term/2.  This uses three other translators:

	* Conditional compilation
	* term_expansion/2 rules provided by the user
	* DCG expansion

Note that this ordering implies  that conditional compilation directives
cannot be generated  by  term_expansion/2   rules:  they  must literally
appear in the source-code.

Term-expansion may choose to overrule DCG   expansion.  If the result of
term-expansion is a DCG rule, the rule  is subject to translation into a
predicate.

Next, the result is  passed  to   expand_bodies/2,  which  performs goal
expansion.
*/

:- dynamic
	system:term_expansion/2,
	system:goal_expansion/2,
	user:term_expansion/2,
	user:goal_expansion/2.
:- multifile
	system:term_expansion/2,
	system:goal_expansion/2,
	user:term_expansion/2,
	user:goal_expansion/2.

:- meta_predicate
	expand_terms(2, +, -).

%%	expand_term(+Input, -Output) is det.
%
%	This predicate is used to translate terms  as they are read from
%	a source-file before they are added to the Prolog database.

expand_term(Var, Expanded) :-
	var(Var), !,
	Expanded = Var.
expand_term(Term, []) :-
	cond_compilation(Term, X),
	X == [], !.
expand_term(Term, Expanded) :-		% local term-expansion
	'$def_modules'(term_expansion/2, MList),
	call_term_expansion(MList, Term, Term2),
	expand_term_2(Term2, Expanded).

call_term_expansion([], Term, Term).
call_term_expansion([M|T], Term0, Term) :-
	(   M:term_expansion(Term0, Term1)
	->  expand_terms(call_term_expansion(T), Term1, Term)
	;   call_term_expansion(T, Term0, Term)
	).

expand_term_2((Head --> Body), Expanded) :-
	dcg_translate_rule((Head --> Body), Expanded0), !,
	expand_bodies(Expanded0, Expanded).
expand_term_2(Term0, Term) :-
	expand_bodies(Term0, Term).

%%	expand_bodies(+Term, -Out) is det.
%
%	Find the body terms in Term and give them to expand_goal/2 for
%	further processing.

expand_bodies(Terms, Out) :-
	'$def_modules'(goal_expansion/2, MList),
	expand_terms(expand_body(MList), Terms, Out).

expand_body(MList, (Head :- Body), (Head :- ExpandedBody)) :-
	nonvar(Body), !,
	expand_goal(Body, ExpandedBody, MList, (Head :- Body)).
expand_body(MList, (:- Body), (:- ExpandedBody)) :-
	nonvar(Body), !,
	expand_goal(Body, ExpandedBody, MList, (:- Body)).
expand_body(_, Head, Head).


%%	expand_terms(:Closure, +In, -Out)
%
%	Loop over two constructs that  can   be  added by term-expansion
%	rules in order to  run  the   next  phase.  Term_expansion/2 can
%	return a list and terms may be preceeded with a source-location.

expand_terms(_, X, X) :-
	var(X), !.
expand_terms(C, [H0|T0], [H|T]) :- !,
	expand_terms(C, H0, H),
	expand_terms(C, T0, T).
expand_terms(C, '$source_location'(File, Line):Clause0,
		'$source_location'(File, Line):Clause) :- !,
	expand_terms(C, Clause0, Clause).
expand_terms(C, Term0, Term) :-
	call(C, Term0, Term).


		 /*******************************
		 *   GOAL_EXPANSION/2 SUPPORT	*
		 *******************************/

%%	expand_goal(+BodyTerm, -Out) is det.
%
%	Perform   macro-expansion   on    body     terms    by   calling
%	goal_expansion/2.

expand_goal(A, B) :-
	'$def_modules'(goal_expansion/2, MList),
	(   expand_goal(A, B, MList, _)
	->  A \== B
	), !.
expand_goal(A, A).

expand_goal(G0, G, MList, Term) :-
	'$set_source_module'(M, M),
	expand_goal(G0, G, M, MList, Term).

%%	expand_goal(+GoalIn, -GoalOut, +Module, -ModuleList, +Term) is det.
%
%	@param Module is the current module to consider
%	@param ModuleList are the other expansion modules
%	@param Term is the overall term that is being translated

% (*)	This is needed because call_goal_expansion may introduce extra
%	context variables.  Consider the code below, where the variable
%	E is introduced.  Is there a better representation for the
%	context?
%
%	  ==
%	  goal_expansion(catch_and_print(Goal), catch(Goal, E, print(E))).
%
%	  test :-
%		catch_and_print(true).
%	  ==

expand_goal(G, G, _, _, _) :-
        var(G), !.
expand_goal(G0, G, M, MList, Term) :-
	call_goal_expansion(MList, G0, G1), !,
	expand_goal(G1, G, M, MList, Term/G1).		% (*)
expand_goal((A,B), Conj, M, MList, Term) :- !,
        expand_goal(A, EA, M, MList, Term),
        expand_goal(B, EB, M, MList, Term),
	simplify((EA, EB), Conj).
expand_goal((A;B), Or, M, MList, Term) :- !,
        expand_goal(A, EA, M, MList, Term),
        expand_goal(B, EB, M, MList, Term),
	simplify((EA;EB), Or).
expand_goal((A->B;C), ITE, M, MList, Term) :- !,
        expand_goal(A, EA, M, MList, Term),
        expand_goal(B, EB, M, MList, Term),
        expand_goal(C, EC, M, MList, Term),
	simplify((EA->EB;EC), ITE).
expand_goal((A->B), (EA->EB), M, MList, Term) :- !,
        expand_goal(A, EA, M, MList, Term),
        expand_goal(B, EB, M, MList, Term).
expand_goal((A*->B), (EA*->EB), M, MList, Term) :- !,
        expand_goal(A, EA, M, MList, Term),
        expand_goal(B, EB, M, MList, Term).
expand_goal((\+A), (\+EA), M, MList, Term) :- !,
        expand_goal(A, EA, M, MList, Term).
expand_goal(call(A), call(EA), M, MList, Term) :- !,
        expand_goal(A, EA, M, MList, Term).
expand_goal(M:G, M:EG, _M, _MList, Term) :-
	atom(M), !,
	'$def_modules'(M:goal_expansion/2, MList),
	setup_call_cleanup('$set_source_module'(Old, M),
			   '$expand':expand_goal(G, EG, M, MList, Term),
			   '$set_source_module'(_, Old)).
expand_goal(G0, G, M, MList, Term) :-
	callable(G0),
	(   default_module(M, M2),
	    '$c_current_predicate'(_, M2:G0),
	    '$get_predicate_attribute'(M2:G0, meta_predicate, Head)
	->  true		% is a meta-pred without clauses defined?
	),
	has_meta_arg(Head),
	expand_meta(Head, G0, G, M, MList, Term),
	G0 \== G, !.
expand_goal(A, A, _, _, _).

expand_meta(Spec, G0, G, M, MList, Term) :-
	functor(Spec, _, Arity),
	functor(G0, Name, Arity),
	functor(G, Name, Arity),
	expand_meta(1, Arity, Spec, G0, G, M, MList, Term).

expand_meta(I, Arity, Spec, G0, G, M, MList, Term) :-
	I =< Arity, !,
	arg(I, Spec, Meta),
	arg(I, G0, A0),
	arg(I, G, A),
	expand_meta_arg(Meta, A0, A, M, MList, Term),
	I2 is I + 1,
	expand_meta(I2, Arity, Spec, G0, G, M, MList, Term).
expand_meta(_, _, _, _, _, _, _, _).

expand_meta_arg(0, A0, A, M, MList, Term) :- !,
	expand_goal(A0, A1, M, MList, Term),
	compile_meta_call(A1, A, M, Term).
expand_meta_arg(^, A0, A, M, MList, Term) :- !,
	expand_setof_goal(A0, A, M, MList, Term).
expand_meta_arg(_, A, A, _, _, _).

has_meta_arg(Head) :-
	arg(_, Head, Arg),
	meta_arg(Arg), !.

meta_arg(0).
meta_arg(^).

expand_setof_goal(Var, Var, _, _, _) :-
	var(Var), !.
expand_setof_goal(V^G, V^EG, M, MList, Term) :- !,
        expand_setof_goal(G, EG, M, MList, Term).
expand_setof_goal(M0:G, M0:EG, M, MList, Term) :- !,
        expand_setof_goal(G, EG, M, MList, Term).
expand_setof_goal(G, EG, M, MList, Term) :- !,
        expand_goal(G, EG0, M, MList, Term),
	compile_meta_call(EG0, EG, M, Term).


%%	call_goal_expansion(+ExpandModules, +Goal0, -Goal) is semidet.
%
%	Succeeds  if  the   context   has    a   module   that   defines
%	goal_expansion/2 this rule succeeds and  Goal   is  not equal to
%	Goal0. Note that the translator is   called  recursively until a
%	fixed-point is reached.

call_goal_expansion(MList, G0, G) :-
	'$member'(M, MList),
	 M:goal_expansion(G0, G),
	 G0 \== G, !.


		 /*******************************
		 *    SIMPLIFICATION ROUTINES	*
		 *******************************/

%%	simplify(+ControlIn, -ControlOut) is det.
%
%	Try to simplify control structure.
%
%	@tbd	Much more analysis
%	@tbd	Turn this into a separate module

simplify(Control, Control) :-
	current_prolog_flag(optimise, false), !.
simplify(Control, Simple) :-
	simple(Control, Simple), !.
simplify(Control, Control).

simple((X, Y), Conj) :-
	(   true(X)
	->  Conj = Y
	;   false(X)
	->  Conj = fail
	;   true(Y)
	->  Conj = X
	).
simple((I->T;E), ITE) :-
	(   true(I)
	->  ITE = T
	;   false(I)
	->  ITE = E
	).
simple((X;Y), Or) :-
	false(X),
	Or = Y.

true(X) :-
	nonvar(X),
	eval_true(X).

false(X) :-
	nonvar(X),
	eval_false(X).

%%	eval_true(+Goal) is semidet.
%%	eval_false(+Goal) is semidet.

eval_true(true).
eval_true(otherwise).

eval_false(fail).
eval_false(false).


		 /*******************************
		 *	   META CALLING		*
		 *******************************/

:- create_prolog_flag(compile_meta_arguments, false, [type(atom)]).

%%	compile_meta_call(+CallIn, -CallOut, +Module, +Term) is det.
%
%	Compile (complex) meta-calls into a clause.

compile_meta_call(CallIn, CallIn, _, Term) :-
	var(Term), !.			% explicit call; no context
compile_meta_call(CallIn, CallIn, _, _) :-
	var(CallIn), !.
compile_meta_call(CallIn, CallIn, _, _) :-
	(   current_prolog_flag(compile_meta_arguments, false)
	;   current_prolog_flag(xref, true)
	), !.
compile_meta_call(CallIn, CallIn, _, _) :-
	strip_module(CallIn, _, Call),
	(   is_aux_meta(Call)
	;   \+ control(Call),
	    (	'$c_current_predicate'(_, system:Call),
		\+ current_prolog_flag(compile_meta_arguments, always)
	    ;   current_prolog_flag(compile_meta_arguments, control)
	    )
	), !.
compile_meta_call(M:CallIn, CallOut, _, Term) :- !,
	(   atom(M), callable(CallIn)
	->  compile_meta_call(CallIn, CallOut, M, Term)
	;   CallOut = M:CallIn
	).
compile_meta_call(CallIn, CallOut, Module, Term) :-
	compile_meta(CallIn, CallOut, Module, Term, Clause),
	Clause = (Head:-Body),
	functor(Head, Name, Arity),
	'$set_source_module'(SM, SM),
	(   current_predicate(SM:Name/Arity)
	->  true
	;   SM == Module
	->  compile_aux_clauses([Clause])
	;   compile_aux_clauses([Head:-Module:Body])
	).

control((_,_)).
control((_;_)).
control((_->_)).
control((_*->_)).
control(\+(_)).

is_aux_meta(Term) :-
	callable(Term),
	functor(Term, Name, _),
	sub_atom(Name, 0, _, _, '__aux_meta_call_').

compile_meta(CallIn, CallOut, M, Term, (CallOut :- Body)) :-
	term_variables(Term, AllVars),
	term_variables(CallIn, InVars),
	intersection_eq(InVars, AllVars, HeadVars),
	variant_sha1(CallIn+HeadVars, Hash),
	atom_concat('__aux_meta_call_', Hash, AuxName),
	expand_goal(CallIn, Body, M, [], (CallOut:-CallIn)),
	length(HeadVars, Arity),
	(   Arity > 256			% avoid 1024 arity limit
	->  HeadArgs = [v(HeadVars)]
	;   HeadArgs = HeadVars
	),
	CallOut =.. [AuxName|HeadArgs].

%%	intersection_eq(+Small, +Big, -Shared) is det.
%
%	Shared are the variables in Small that   also appear in Big. The
%	variables in Shared are in the same order as Shared.

intersection_eq([], _, []).
intersection_eq([H|T0], L, List) :-
	(   member_eq(H, L)
	->  List = [H|T],
	    intersection_eq(T0, L, T)
	;   intersection_eq(T0, L, List)
	).

member_eq(E, [H|T]) :-
	(   E == H
	->  true
	;   member_eq(E, T)
	).


		 /*******************************
		 *	:- IF ... :- ENDIF	*
		 *******************************/

:- thread_local
	'$include_code'/3.

'$including' :-
	'$include_code'(X, _, _), !,
	X == true.
'$including'.

cond_compilation((:- if(G)), []) :-
	source_location(File, Line),
	(   '$including'
	->  (   catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true, File, Line))
	    ;   asserta('$include_code'(false, File, Line))
	    )
	;   asserta('$include_code'(else_false, File, Line))
	).
cond_compilation((:- elif(G)), []) :-
	source_location(File, Line),
	(   clause('$include_code'(Old, OF, _), _, Ref)
	->  same_source(File, OF, elif),
	    erase(Ref),
	    (   Old == true
	    ->  asserta('$include_code'(else_false, File, Line))
	    ;   Old == false,
		catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true, File, Line))
	    ;	asserta('$include_code'(Old, File, Line))
	    )
	;   throw(error(conditional_compilation_error(no_if, elif), _))
	).
cond_compilation((:- else), []) :-
	source_location(File, Line),
	(   clause('$include_code'(X, OF, _), _, Ref)
	->  same_source(File, OF, else),
	    erase(Ref),
	    (   X == true
	    ->  X2 = false
	    ;   X == false
	    ->	X2 = true
	    ;	X2 = X
	    ),
	    asserta('$include_code'(X2, File, Line))
	;   throw(error(conditional_compilation_error(no_if, else), _))
	).
cond_compilation(end_of_file, end_of_file) :- !, % TBD: Check completeness
	source_location(File, _),
	(   clause('$include_code'(_, OF, OL), _)
	->  (   File == OF
	    ->	throw(error(conditional_compilation_error(
				unterminated,OF:OL), _))
	    ;   true
	    )
	;   true
	).
cond_compilation((:- endif), []) :- !,
	source_location(File, _),
	(   (   clause('$include_code'(_, OF, _), _, Ref)
	    ->  same_source(File, OF, endif),
		erase(Ref)
	    )
	->  true
	;   throw(error(conditional_compilation_error(no_if, endif), _))
	).
cond_compilation(_, []) :-
	\+ '$including'.

same_source(File, File, _) :- !.
same_source(_,    _,    Op) :-
	throw(error(conditional_compilation_error(no_if, Op), _)).


'$eval_if'(G) :-
	expand_goal(G, G2),
	'$set_source_module'(Module, Module),
	Module:G2.
