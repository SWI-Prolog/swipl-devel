/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(test_abolish_active_table,
          [ test_abolish_active_table/0
          ]).
:- use_module(library(debug)).

%!  test_abolish_active_table
%
%   Verify that we can abolish an  incomplete   table  and still get the
%   correct result.

test_abolish_active_table :-
    abolish_all_tables,
    findall(X, p(X), Xs),
    ok(Xs).

:- table (q/1, p/1) as shared.

max(10).

p(Y) :- sleep(0.01), q(X), max(Max), X < Max, Y is X+1.
p(0).
q(Y) :- sleep(0.01), p(X), max(Max), X < Max, a(X), Y is X+1.
q(0).

a(5) :-
    !,
    abolish_all_tables.
a(_).

%!  ok(+Xs)
%
%   Validate the answer.  If wrong, abort the process.

ok(Xs) :-
    sort(Xs, Sorted),
    max(Max),
    numlist(0, Max, Sorted),
    !.
ok(Xs) :-
    debug(wrong, 'WRONG: ~q', [Xs]),
    fail.

:- debug(wrong).
