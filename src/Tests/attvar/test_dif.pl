/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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
*/

:- module(test_dif,
	  [ test_dif/0
	  ]).

:- '$clausable'(dif/1).

dif(1) :-
	dif(1, A), \+ A = 1.
dif(2) :-
	dif(1, A), dif(2, A), \+ A = 1.
dif(3) :-
	dif(1, A), dif(2, A), \+ A = 2.
dif(4) :-
	dif(A, B), A = 1, \+ B = 1.
dif(5) :-
	A = a(A, 1),
	B = a(B, X),
	dif(A, B), \+ X = 1.
dif(6) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = a,
	\+ attvar(B).
dif(7) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = x(1,2),
	\+ B = 1.
dif(8) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = x(1,Y),
	Y = 3,
	\+ attvar(B).
dif(9) :-
	dif(X, Y), \+ X = Y.
dif(10) :-
	dif(f(X,_Z),f(a,b)),
	dif(f(X,Y),f(b,b)),
	X = a, Y = b.
dif(11) :-
	dif(A,B), memberchk(A, [B, C]),
	A == C.
dif(12) :-		% https://github.com/SWI-Prolog/issues/issues/15
	dif(X-Y,1-2), X=Y, Y = 1.
dif(13) :-		% https://github.com/SWI-Prolog/issues/issues/15
	dif(X-Y,1-2), X=Y, Y = 2.

:- dynamic
	failed/1.

test_dif :-
	retractall(failed(_)),
	forall(clause(dif(N), _, _),
	       (   dif(N)
	       ->  true
	       ;   format('~NFailed: ~w~n', [dif(N)]),
		   assert(failed(N))
	       )),
	\+ failed(_).

