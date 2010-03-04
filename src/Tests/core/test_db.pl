/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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

:- module(test_db, [test_db/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core database functions

@author	Jan Wielemaker
*/

test_db :-
	run_tests([ assert,
		    dynamic,
		    res_compiler
		  ]).

:- begin_tests(assert).

:- dynamic
	term/0,
	f/1, f/2.

test(right_cyclic_head, [ sto(rational_trees),
			  error(representation_error(cyclic_term))
			]) :-
	X = f(X),
	assert(X).
test(cyclic_head, [ sto(rational_trees),
			  error(representation_error(cyclic_term))
			]) :-
	X = f(X, 1),
	assert(X).
test(cyclic_body, [ sto(rational_trees),
		    error(representation_error(cyclic_term))
		  ]) :-
	X = f(X),
	assert((f(a) :- X)).

:- end_tests(assert).

:- begin_tests(dynamic).

test(make_dynamic, [true, cleanup(abolish(Name, 1))]) :-
	gensym(somepred, Name),
	Term =.. [Name, a],
	catch(Term, _, true),
	assertz(Term),
	Term.

:- end_tests(dynamic).

:- begin_tests(res_compiler).

:- dynamic
        tmp/2.

test_big_clause(N) :-
        link_clause(N, V0, V, Body),
        retractall(tmp(_,_)),
        Clause = (tmp(V0, V) :- Body),
        assert(Clause, Ref),
        tmp(0, X),
        erase(Ref),
        assertion(X == N).

link_clause(1, V0, V, succ(V0, V)) :- !.
link_clause(N, V0, V, (succ(V0, V1), G)) :-
        N2 is N - 1,
        link_clause(N2, V1, V, G).

% (*) Variable count is represented as an unsigned short.  Note that
% this error may disappear if we do smarter variable allocation.

test(big_clause,
     [ true,
       cleanup(trim_stacks)
     ]) :-
	test_big_clause(60000).
test(too_big_clause,			% (*)
     [ error(representation_error(max_frame_size)),
       cleanup(trim_stacks)
     ]) :-
	test_big_clause(80000).

:- end_tests(res_compiler).
