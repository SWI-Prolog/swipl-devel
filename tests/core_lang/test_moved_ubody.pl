/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(test_moved_ubody,
          [ test_moved_ubody/0
          ]).
:- use_module(library(plunit)).

/** <module> Test unifications that have been moved to the head
*/

test_moved_ubody :-
    run_tests([ moved_unify_ubody,
                moved_decompile
              ]).

:- begin_tests(moved_unify_ubody).

ol_append1(OL1, OL2, Elem) :-
    OL1 = ol(List1, [Elem|Tail2]),
    OL2 = ol(List1, Tail2).

test(h_list_ff, L =@= ol([a,[b|X]],X)) :-
    ol_append1(ol([a,T], T), L, b).

:- end_tests(moved_unify_ubody).

:- begin_tests(moved_decompile).

nf_add([B|Bs], CLP, A, As, Cs) :-
	A = v(Ka,Pa),
	B = v(Kb,Pb),
	compare(Rel,Pa,Pb),
	nf_add_case(Rel,CLP,A,As,Cs,B,Bs,Ka,Kb,Pa).

s1([V|K],A,B,C) :-
	A = v(X),
	q(V,K,A,B,C,X).

s2([V|K],A,B,C) :-
	B = v(X),
	q(V,K,A,B,C,X).

s3(A,B,C) :-
	A = v(X),
	q(A,B,C,X).

s4(X, X) :-
     X = f(_).

s5(X) :-
    X = f(X).

s6(X) :-
    X = f(_),
    q([X]).

s7(X) :-
    X = f(A),
    q(A).

nf_add_case(_Rel,_CLP,_A,_As,_Cs,_B,_Bs,_Ka,_Kb,_Pa).
q(_).
q(_,_,_,_).
q(_,_,_,_,_,_).

s8(D, C) :-
    D = since(_,_),
    C = D.

s9(X, Y) :-
    X = f(Y),
    Y = a.

test(decom1, (Head :- Body) =@= (nf_add([B|Bs], CLP, A, As, Cs) :-
                                    A = v(Ka,Pa),
                                    B = v(Kb,Pb),
                                    compare(Rel,Pa,Pb),
                                    nf_add_case(Rel,CLP,A,As,Cs,B,Bs,Ka,Kb,Pa))) :-
    Head = nf_add(_,_,_,_,_),
    clause(Head, Body).
test(decomp2, (Head :- Body) =@= (s1([V|K],A,B,C) :-
                                     A = v(X),
                                     q(V,K,A,B,C,X))) :-
    Head = s1(_,_,_,_),
    clause(Head, Body).
test(decomp3, (Head :- Body) =@= (s2([V|K],A,B,C) :-
                                     B = v(X),
                                     q(V,K,A,B,C,X))) :-
    Head = s2(_,_,_,_),
    clause(Head, Body).
test(decomp4, (Head :- Body) =@= (s3(A,B,C) :-
                                     A = v(X),
                                     q(A,B,C,X) )) :-
    Head = s3(_,_,_),
    clause(Head, Body).
test(decomp5, (Head :- Body) =@= (s4(A,A) :-
                                     A = f(_) )) :-
    Head = s4(_,_),
    clause(Head, Body).
test(decomp6, (Head :- Body) =@= (s5(A) :-
                                     A = f(A) )) :-
    Head = s5(_),
    clause(Head, Body).
test(decomp7, (Head :- Body) =@= (s6(A) :-
                                     A = f(_),
                                     q([A]))) :-
    Head = s6(_),
    clause(Head, Body).
test(decomp8, (Head :- Body) =@= (s7(f(A)) :-
                                     q(A))) :-
    Head = s7(_),
    clause(Head, Body).
test(decomp9, (Head :- Body) =@= (s8(A,B) :- A=since(_,_),B=A)) :-
    Head = s8(_,_),
    clause(Head, Body).
test(run9, X+Y == f(a)+a) :-
    s9(X,Y).
test(decomp9, (Head :- Body) =@= (s9(f(Y),Y) :- Y = a)) :-
    Head = s9(_,_),
    clause(Head, Body).

:- end_tests(moved_decompile).
