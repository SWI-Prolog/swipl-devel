/*  Part of SWI-Prolog

    Author:        Ulrich Neumerkel and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
                         VU University Amsterdam
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

Note that the when/2 tests are written by Ulrich Neumerkel and subject
to the GPL-2.0-or-later license.
*/


:- module(test_coroutining,
          [test_coroutining/0]).
:- use_module(library(clpfd)).
:- use_module(library(plunit)).

/** <module> Test coroutining primitives

This module is a Unit test for  freeze/2, etc.
*/

test_coroutining :-
    run_tests([ coroutining
	      ]).

:- begin_tests(coroutining).

% removal of duplicates, nested delays and variable identity
% Should the module be stripped, refer to the current module (now) or
% the implementation module?
test(frozen, G == (freeze(X, M:writeln(x)), freeze(Y, M:write(X-Z)))) :-
    context_module(M),
    freeze(X, writeln(x)),
    freeze(Y, write(X-Z)),
    freeze(Y, write(X-Z)),
    frozen(Y, G).
test(frozen_diff, Frozen == Frozen2) :-
    dif(X, Y),
    frozen([X, Y], Frozen),
    frozen([X, Y], Frozen2).
test(frozen_clpfd, Frozen == Frozen2) :-
    Y #= X + 1, frozen([X, Y], Frozen),
    Y #= X + 1, frozen([X, Y], Frozen2).

% when/2 tests by Ulrich Neumerkel.  These tests are only available
% under the GPL-2 license.

test(when1, [error(instantiation_error)]) :-
	when(_,1=1).
test(when2,[error(instantiation_error)]) :-
	when((_,_),1=2).
test(when3,[error(instantiation_error)]) :-
	when((nonvar(_),_),1=2).
test(when4_inf,[sto(rational_trees), error(type_error(_,_))]) :-
	C=(C,C),
	when(C,1=2).
test(when5_r,[true(X==2)]) :-
	when(ground(g),X=2).
test(when6,[error(domain_error(_,_))]) :-
	when(true, 1=2).
test(when7,[true((R,S)==(est,sunt))]) :-
	when((nonvar(X);nonvar(Y)),R = est),
	when((nonvar(Y),nonvar(X)),S = sunt),
	(X,Y)=(a,a).
test(when8,[fail]) :-
	when(ground(g),fail).
test(when8,X==a) :-
	v(A),
	when(((nonvar(A), (nonvar(B) ; nonvar( C)))
	     ;(nonvar(B), nonvar(C))), X = a),
	B=2, C=3.

v(_).

:- end_tests(coroutining).
