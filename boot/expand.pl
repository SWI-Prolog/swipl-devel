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

:- module('$expand',
	  [ expand_term/2,
	    expand_goal/2
	  ]).

/** <module> Prolog source-code transformation

This module specifies, together with dcg.pl, the transformation of terms
as they are read from a file before they are processed by the compiler.

The toplevel is expand_term/2.  This uses three other translators:

	* Conditional compilation
	* Goal expansion
	* DCG expansion
*/

:- dynamic
	user:term_expansion/2,
	user:goal_expansion/2.
:- multifile
	user:term_expansion/2,
	user:goal_expansion/2.

%%	expand_term(+Input, -Output) is det.
%
%	This predicate is used to translate terms  as they are read from
%	a source-file before they are added to the Prolog database.

expand_term(Var, Expanded) :-
	var(Var), !,
	Expanded = Var.
expand_term(Term, Expanded) :-		% local term-expansion
	'$term_expansion_module'(Module),
	Module:term_expansion(Term, Expanded0), !,
	expand_bodies(Expanded0, Expanded).
expand_term(Head --> Body, Expanded) :-
	dcg_translate_rule(Head --> Body, Expanded0), !,
	expand_bodies(Expanded0, Expanded).
expand_term(Term, []) :-
	'$if_expansion'(Term, X),
	X == [], !.
expand_term(Term0, Term) :-
	expand_bodies(Term0, Term).

%%	expand_bodies(+Term, -Out) is det.
%
%	Find the body terms in Term and give them to expand_goal/2 for
%	further processing.

expand_bodies(Terms, Out) :-
	'$goal_expansion_module'(_), !,
	expand_bodies_2(Terms, Out).
expand_bodies(Terms, Terms).


expand_bodies_2(X, X) :-
	var(X), !.
expand_bodies_2([H0|T0], [H|T]) :- !,
	expand_bodies_2(H0, H),
	expand_bodies_2(T0, T).
expand_bodies_2('$source_location'(File, Line):Clause0,
		  '$source_location'(File, Line):Clause) :- !,
	expand_bodies_2(Clause0, Clause).
expand_bodies_2((Head :- Body), (Head :- ExpandedBody)) :-
	nonvar(Body), !,
	expand_goal(Body, ExpandedBody).
expand_bodies_2((:- Body), (:- ExpandedBody)) :-
	nonvar(Body), !,
	expand_goal(Body, ExpandedBody).
expand_bodies_2(Head, Head).


		 /*******************************
		 *   GOAL_EXPANSION/2 SUPPORT	*
		 *******************************/

%%	expand_goal(+BodyTerm, -Out) is det.
%
%	Perform   macro-expansion   on    body     terms    by   calling
%	goal_expansion/2.

expand_goal(A, B) :-
        '$do_expand_body'(A, B0),
	'$tidy_body'(B0, B).

'$do_expand_body'(G, G) :-
        var(G), !.
'$do_expand_body'((A,B), (EA,EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((A;B), (EA;EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((A->B), (EA->EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((A*->B), (EA*->EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'((\+A), (\+EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(A, B) :-
        '$goal_expansion_module'(M),
        M:goal_expansion(A, B0),
	B0 \== A, !,			% avoid a loop
	'$do_expand_body'(B0, B).
'$do_expand_body'(not(A), not(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(call(A), call(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(once(A), once(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(ignore(A), ignore(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(initialization(A), initialization(EA)) :- !,
        '$do_expand_body'(A, EA).
'$do_expand_body'(catch(A, E, B), catch(EA, E, EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'(call_cleanup(A, B), call_cleanup(EA, EB)) :- !,
        '$do_expand_body'(A, EA),
	'$do_expand_body'(B, EB).
'$do_expand_body'(call_cleanup(A, R, B), call_cleanup(EA, R, EB)) :- !,
        '$do_expand_body'(A, EA),
	'$do_expand_body'(B, EB).
'$do_expand_body'(forall(A, B), forall(EA, EB)) :- !,
        '$do_expand_body'(A, EA),
        '$do_expand_body'(B, EB).
'$do_expand_body'(findall(V, G, B), findall(V, EG, B)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(findall(V, G, B, T), findall(V, EG, B, T)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(bagof(V, G, B), bagof(V, EG, B)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(setof(V, G, B), setof(V, EG, B)) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(V^G, V^EG) :- !,
        '$do_expand_body'(G, EG).
'$do_expand_body'(M:G, M:EG) :-
	atom(M), !,
	(   M == system			% or should we define this:
	->  EG = G			% system:goal_expansion(X,X)
	;   '$set_source_module'(Old, M),
	    call_cleanup('$do_expand_body'(G, EG),
			 '$set_source_module'(_, Old))
	).
'$do_expand_body'(A, A).

%	Delete extraneous true's that result from goal_expansion(..., true)
%
%	Is the really necessary?  Should we only do it if -O is effective?

'$tidy_body'(A, A) :-
	current_prolog_flag(optimise, false), !.
'$tidy_body'(A, A) :-
        var(A), !.
'$tidy_body'((A,B), (A, TB)) :-
        var(A), !,
        '$tidy_body'(B, TB).
'$tidy_body'((A,B), (TA, B)) :-
        var(B), !,
        '$tidy_body'(A, TA).
'$tidy_body'(((A,B),C), R) :- !,
	'$tidy_body'((A,B,C), R).
'$tidy_body'((true,A), R) :- !,
        '$tidy_body'(A, R).
'$tidy_body'((A,true), R) :- !,
        '$tidy_body'(A, R).
'$tidy_body'((A,B), (TA, TB)) :- !,
        '$tidy_body'(A, TA),
        '$tidy_body'(B, TB).
'$tidy_body'((A;B), (TA; TB)) :- !,
        '$tidy_body'(A, TA),
        '$tidy_body'(B, TB).
'$tidy_body'((A->B), (TA->TB)) :- !,
        '$tidy_body'(A, TA),
        '$tidy_body'(B, TB).
'$tidy_body'(A, A).

		 /*******************************
		 *	:- IF ... :- ENDIF	*
		 *******************************/

:- thread_local
	'$include_code'/1.

'$including' :-
	'$include_code'(X), !,
	X == true.
'$including'.

'$if_expansion'((:- if(G)), []) :-
	(   '$including'
	->  (   catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true))
	    ;   asserta('$include_code'(false))
	    )
	;   asserta('$include_code'(else_false))
	).
'$if_expansion'((:- elif(G)), []) :-
	(   retract('$include_code'(Old))
	->  (   Old == true
	    ->  asserta('$include_code'(else_false))
	    ;   Old == false,
		catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true))
	    ;	asserta('$include_code'(Old))
	    )
	;    throw(error(context_error(no_if), _))
	).
'$if_expansion'((:- else), []) :-
	(   retract('$include_code'(X))
	->  (   X == true
	    ->  X2 = false
	    ;   X == false
	    ->	X2 = true
	    ;	X2 = X
	    ),
	    asserta('$include_code'(X2))
	;   throw(error(context_error(no_if), _))
	).
'$if_expansion'(end_of_file, end_of_file) :- !. % TBD: Check completeness
'$if_expansion'((:- endif), []) :-
	retract('$include_code'(_)), !.

'$if_expansion'(_, []) :-
	\+ '$including'.

'$eval_if'(G) :-
	expand_goal(G, G2),
	'$set_source_module'(Module, Module),
	Module:G2.
