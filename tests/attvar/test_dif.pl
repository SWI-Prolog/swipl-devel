/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2023, University of Amsterdam
                              VU University Amsterdam
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

:- module(test_dif,
	  [ test_dif/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(dif)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).

test_dif :-
	run_tests([ dif
		  ]).

:- begin_tests(dif).

test(1) :-
	dif(1, A), \+ A = 1.
test(2) :-
	dif(1, A), dif(2, A), \+ A = 1.
test(3) :-
	dif(1, A), dif(2, A), \+ A = 2.
test(4) :-
	dif(A, B), A = 1, \+ B = 1.
test(5, [sto(rational_trees)]) :-
	A = a(A, 1),
	B = a(B, X),
	dif(A, B), \+ X = 1.
test(6) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = a,
	\+ attvar(B).
test(7) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = x(1,2),
	\+ B = 1.
test('7b') :-
	dif(a(x(1,2), B), a(X, 1)),
	X = x(1,2),
	B \= 1.
test(8, [sto(rational_trees)]) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = x(1,Y),
	Y = 3,
	\+ attvar(B).
test(9) :-
	dif(X, Y), \+ X = Y.
test(10) :-
	dif(f(X,_Z),f(a,b)),
	dif(f(X,Y),f(b,b)),
	X = a, Y = b.
test(res1, L == []) :-
	call_residue_vars((dif(a(_,B), a(_,1)), B = 2), L).
test(11) :-
	dif(A,B), memberchk(A, [B, C]),
	A == C.
test(12) :-		% https://github.com/SWI-Prolog/issues/issues/15
	dif(X-Y,1-2), X=Y, Y = 1.
test(13) :-		% https://github.com/SWI-Prolog/issues/issues/15
	dif(X-Y,1-2), X=Y, Y = 2.
test(14) :-
	P = t3(A,A),
	A = t5(D,D),
	D = t8(c,b),

	Q = t3(t5(t8(_,b),t8(c,b)),
	       t5(G,_)),

	dif(P,Q),
	G = t8(x,b),
	assertion(term_attvars(P+Q, [])).
test(15, [sto(rational_trees)]) :-
	P2 = t123(t124(A),A),
	A  = t125(t126(a,t127(B1))),
	Q2 = t123(C1,t125(t126(a,t127(D1)))),

	dif(P2,Q2),

	C1 = t124(t125(t126(a,E1))),
	D1 = B1,
	E1 = t127(x),

	\+ B1 = x.	% this should fail
test(16) :-
	dif(_A-C,_B-D),
	C-D=z-z.
test(no_dup, [P==[x, y, z, z], nondet]) :-
	permutation_no_dup([x,y,Z,Z],P), P=[x,y,z,z].
test(17) :-		% from Issue#17
	dif(A,[_|B]),A=[[]|_],A=[B].
test(other_atts) :-
	call_residue_vars((
		freeze(X, XDone = true),
		freeze(Y, YDone = true),
		dif(A, B),
		X = A,
		Y = B,
		\+ X = Y,
		X = 1,
		\+ Y = 1,
		Y = 2
	), Vars),
	Vars == [],
	XDone == true,
	YDone == true.
test(issue122) :-
	A=[B|_],
	C=[_|_],
	dif(C, A),
	C=[B|B].
test(issue109, [sto(rational_trees)]) :-
	A=[B|A], C=[D|B], dif(A, C), A=[D|A],
	attvar(D).
% See https://github.com/SWI-Prolog/issues/issues/113#issue-1234908231
test(cyclic, blocked("Cyclic term")) :-
	dif(A, B),
	C=[D|D],
	A=[D|E],
	B=[C|D],
	D=[E|E].
test(issue122, Vars == []) :-
    A=x(a,C), B=x(D,A), C = x(_,_),
    call_residue_vars(( dif(A,B),
			C=x(_E,D)
		      ), Vars).

:- end_tests(dif).

% From issue#105

permutation_no_dup([], []).
permutation_no_dup(L, PL):-
    same_length(L, PL),
    length(L, Len),
    numlist(1,Len, RLMax),
    reverse(RLMax, LMax),
    length(LCur, Len),
    maplist(=(1), LCur),
    permutation_no_dup(LCur, L, LMax/LCur-L, [], PL).

permutation_no_dup([], _, _, PL, PL).
permutation_no_dup([], _, LMax/LCur-L, PL, PL1):-
    next(LCur, LMax, NLCur),
    dif(PL, PL1),
    permutation_no_dup(NLCur, L, LMax/NLCur-L, [], PL1).
permutation_no_dup([Take|LCur], L, Info, PL, PL1):-
    nth1(Take, L, Item, L1),
    permutation_no_dup(LCur, L1, Info, [Item|PL], PL1).

next([Cur|LCur], [Max|_], [NCur|LCur]):-
    Cur < Max,
    NCur is Cur+1.
next([Cur|LCur], [Cur|LMax], [1|NLCur]):-
    next(LCur, LMax, NLCur).
