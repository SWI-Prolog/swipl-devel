/*  $Id$

    Part of SWI-Prolog

    Author:        Ulrich Neumerkel
    WWW:           http://www.swi-prolog.org
    Copyright (C): Ulrich Neumerkel

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


:- module(test_dcg, [test_dcg/0]).
:- use_module(library(plunit)).
:- use_module(library(apply_macros)).

test_dcg :-
	run_tests([ phrase,
		    phrase_expansions,
		    rule_expansions,
		    dcg_rule_expansions
		  ]).

:- begin_tests(phrase).

test(iso_8_1_1_3, [error(instantiation_error)]) :-
	phrase(_,[],[]).
test(iso_8_1_1_3, [error(instantiation_error)]) :-
	phrase(L,L,L).
test(iso_8_1_1_3_OPEN, [error(type_error(_,27))]) :-
	phrase(27,[],[]).
test(iso_8_1_1_3_OPEN, [error(type_error(callable,27))]) :-
	phrase(27,[],[]).
%test(iso_8_1_1_3, [ blocked(disagreement_with_ISO_DTR_draft),
%		    error(type_error(list,_))
%		  ]) :-
%	phrase([],a).
test(iso1,[]) :-
	phrase([the],[the]).
test(uwn,[error(instantiation_error)]) :-
	phrase(L,L).
test(uwn,[L0 == L]) :-
	phrase([],L0,L).
%test(uwn,[sto(rational_trees)]) :- % This should test the occurs check implicitly
%	phrase([1],L,L).

:- end_tests(phrase).

:- begin_tests(phrase_expansions).

test(1, [G == a(L,[])]) :-
	expand_goal(phrase(a,L), G).
test(2, [B == (b([x], []), []=[1])]) :-
	expand_goal(phrase({phrase(b,[x])},[1]),B).

:- end_tests(phrase_expansions).


:- begin_tests(rule_expansions).

test(1, [R == (a :- b([1],[]))]) :-
	expand_term((a :- phrase(b,[1])),R).

:- end_tests(rule_expansions).

:- begin_tests(dcg_rule_expansions).

test(1, [R =@= (a(X0,X) :- b(X0,X1), c(X1,X))]) :-
	expand_term((a --> b, c), R).
test(meta1,[R =@= (a([1,2|L],[3|L]):-true)]) :-
	expand_term((a,[3]-->[1,2]),R).

:- end_tests(dcg_rule_expansions).
