/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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
:- module(test_compare_cyclic, [test_compare_cyclic/0]).
:- use_module(library(plunit)).

/** <module> Test the standard order on cyclic terms

The standard order compares arguments left to right, where two arguments
that are equal (==/2) as rational trees are skipped.  For many pairs of
cyclic terms this defines the order, and then compare/3 must find it
independently of how the terms are shared in memory.  For some pairs,
e.g., A = s(B,0), B = s(A,1), no consistent order exists.  Then we only
demand that the comparison terminates.
*/

test_compare_cyclic :-
    run_tests([ compare_cyclic
              ]).

:- begin_tests(compare_cyclic, [sto(rational_trees)]).

% The terms from #1529: X = f(X,0), Y = f(Y,1), Z = f(X,Y)

terms(X, Y, Z) :-
    X = f(X,0),
    Y = f(Y,1),
    Z = f(X,Y).

copies(A, B, C) :-
    terms(X, Y, Z),
    copy_term(X, A),
    copy_term(Y, B),
    copy_term(Z, C).

test(shared, [O1,O2] == [<,<]) :-
    terms(X, Y, Z),
    compare(O1, X, Z),
    compare(O2, Z, Y).
test(copied, [O1,O2] == [<,<]) :-
    copies(A, B, C),
    compare(O1, A, C),
    compare(O2, C, B).
test(mixed, [O1,O2] == [<,<]) :-
    terms(X, _, Z),
    copies(A, _, C),
    compare(O1, X, C),
    compare(O2, A, Z).
test(sort, L == [A,C,B]) :-
    copies(A, B, C),
    sort([A,B,C], L).
test(sort_reverse, L == [A,C,B]) :-
    copies(A, B, C),
    sort([C,B,A], L).
test(partial, O == undecided(0, V)) :-
    X = f(X,0),
    copy_term(X, A),
    C = f(X,V),
    partial_compare(O, A, C).
test(no_order) :-
    A = s(B,0),
    B = s(A,1),
    compare(O1, A, B),
    copy_term(A-B, A1-B1),
    compare(O2, A1, B),
    compare(O3, A, B1),
    maplist(order, [O1,O2,O3]).

order(<).
order(>).

% Acyclic terms whose comparison follows links must not use the slow
% path.  Before this was fixed, this took minutes.
test(shared_acyclic, [O == (<), true(T < 5)]) :-
    X = g(h), Y = g(h),
    shared_list(200 000, X, 1, L1),
    shared_list(200 000, Y, 2, L2),
    statistics(cputime, T0),
    compare(O, L1, L2),
    statistics(cputime, T1),
    T is T1-T0.

shared_list(N, E, End, L) :-
    length(L0, N),
    maplist(=(E), L0),
    append(L0, [End], L).

:- end_tests(compare_cyclic).
