/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2021, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

:- module(test_attvar, [test_attvar/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog attvaring

This module is a Unit test for Prolog unification oddities.  If basic
unification is wrong you won't get as far as running this test :-)

@author	Jan Wielemaker
*/

test_attvar :-
    run_tests([ attvar,
		freeze,
		attvar_with_occurs_check
	      ]).

:- begin_tests(attvar).

test(s_list, L==Codes) :-		% Verify wakeup on S_LIST
    string_codes("hello", Codes),
    freeze(X, X=Codes),
    append(X, [], L).
test(true_ndet, error(existence_error(procedure,_))) :-
    freeze(X, wake(X)),
    between(-2, 2, X).

wake(2) :-
	i_am_undefined.

:- end_tests(attvar).

:- begin_tests(freeze).

test(freeze_and, true) :-
    freeze(X, true),
    freeze(Y, true),
    X=Y,
    freeze(X, true),
    X=a.
test(freeze_memberchk, L == [1,2,3]) :-
    freeze(L, L=[1,2,3]),
    memberchk(2, L).
test(freeze_memberchk, L == [1,2,3]) :-
    freeze(L, L=[1,2,3]),
    memberchk(2, [a,b|L]).
test(freeze_memberchk, E-L == 1-[1,2,3]) :-
    freeze(L, L=[1,2,3]),
    memberchk(E, L).
test(freeze_memberchk, E == 2) :-
    freeze(E, E = 2),
    memberchk(2, [1,E]).

:- end_tests(freeze).

:- begin_tests(attvar_with_occurs_check).

test(occurs_check, fail) :-
    freeze(A, writeln(A)),
    freeze(B, writeln(B)),
    Q=[[[A],[B]],x],
    P=[A|B],
    unify_with_occurs_check(P, Q).

:- end_tests(attvar_with_occurs_check).
