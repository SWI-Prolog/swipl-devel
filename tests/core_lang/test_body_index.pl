/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(test_body_index,
          [ test_body_index/0
          ]).
:- use_module(library(plunit)).

test_body_index :-
    run_tests([ body_index
              ]).

:- begin_tests(body_index).

:- discontiguous
    source/2.

term_expansion(case(Head, Clause),
    [ Clause,
      source(Head, Clause)
    ]).
term_expansion(case(Head, Clause, Expected),
    [ Clause,
      source(Head, Expected)
    ]).

case(p1(_),
     (p1(X) :- X = f(_), q(X))).
case(p2(_),
     (p2(X) :- f(_) = X, q(X)),
     (p2(X) :- X = f(_), q(X))).
case(p3(_,_),
     (p3(X,X) :- X = f(_), q(X))).
case(p4(_,_),
     (p4(X,a) :- X = x, q(X))).
case(p5(_,_),
     (p5(X,a) :- X = f(_), q(X))).
case(p6(_,_),
     (p6(a,X) :- X = f(_), q(X))).
case(p7(_,_,_),
     (p7(X,A,B) :- X = f(_), q(X), A == B)).
case(p8(_,_,_,_),
     (p8(X,A,B,a) :- X = f(_), q(X), A == B)).

q(f(a)).

test_clause(Head) :-
    source(Head, Expected),
    clause(Head, Body),
    ok_clause(Head, (Head :- Body), Expected).

ok_clause(_, Decompiled, Expected) :-
    Decompiled =@= Expected, !.
ok_clause(Head, Decompiled, Expected) :-
    format(user_error, '~N~p: Expected:~n', [Head]),
    portray_clause(user_error, Expected, [indent(4)]),
    format(user_error, 'Decompiled:~n', []),
    portray_clause(user_error, Decompiled, [indent(4)]),
    fail.

test(p1) :- test_clause(p1(_)).
test(p2) :- test_clause(p2(_)).
test(p3) :- test_clause(p3(_,_)).
test(p4) :- test_clause(p4(_,_)).
test(p5) :- test_clause(p5(_,_)).
test(p6) :- test_clause(p6(_,_)).
test(p6) :- \+ p6(_,f(b)).
test(p7) :- test_clause(p7(_,_,_)).
test(p8) :- test_clause(p8(_,_,_,_)).

:- end_tests(body_index).
