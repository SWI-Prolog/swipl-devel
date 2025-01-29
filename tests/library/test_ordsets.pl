/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2013, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_ordsets,
	  [ test_ordsets/0
	  ]).


:- use_module(library(plunit)).
:- use_module(library(ordsets)).

test_ordsets :-
	run_tests([ ord_intersection,
		    is_ordset
		  ]).

:- begin_tests(ord_intersection).

test(oint4, [X,Y] == [[],[]]) :-
	ord_intersection([], [], X, Y).
test(oint4, [X,Y] == [[b],[]]) :-
	ord_intersection([a,b,c], [b], X, Y).
test(oint4, [X,Y] == [[a],[b,c]]) :-
	ord_intersection([a], [a,b,c], X, Y).
test(oint4, [X,Y] == [[b],[a,c]]) :-
	ord_intersection([b], [a,b,c], X, Y).
test(oint4, [X,Y] == [[c],[a,b]]) :-
	ord_intersection([c], [a,b,c], X, Y).


:- end_tests(ord_intersection).

:- begin_tests(is_ordset).

test(is_ordset, true) :-
	is_ordset([a,b,c]).
test(is_ordset, [sto(rational_trees), fail]) :-
	L = [a|L],
	is_ordset(L).
test(is_ordset, fail) :-		% This is why we cannot use sort(X,X)
	unbound(T),			% do not optimize unification
	T = a(X,Y),
	is_ordset([Y,X]).

unbound(_).

:- end_tests(is_ordset).
