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


:- module(test_abolish_depending_table,
          [ test_abolish_depending_table/0
          ]).
:- use_module(library(plunit)).

/** <module> Abolish a table other tables delay on

Abolishing a table destroys the  tables that have conditional answers
delaying on  it (destroy_depending_worklists()).   If such a  table is
under evaluation it may  not be destroyed right away:  that frees the
worklist the running evaluation is using.
*/

test_abolish_depending_table :-
    run_tests([ abolish_depending_table
              ]).

:- begin_tests(abolish_depending_table).

:- table q/1, u/1, p/1.
:- dynamic d/1.

q(X) :- d(X), tnot(u(X)).
u(X) :- d(X), tnot(q(X)).

%  The first clause gives p/1 conditional answers that delay on q/1;
%  the second abolishes q/1 while p/1 is still being evaluated.

p(X) :- q(X).
p(0) :- abolish_table_subgoals(q(_)).

test(abolish_from_inside_depending_evaluation, Xs == [0,1,2]) :-
    abolish_all_tables,
    retractall(d(_)),
    assertz(d(1)), assertz(d(2)),
    findall(X, p(X), Xs0),
    msort(Xs0, Xs).

%  The same abolish from outside an evaluation destroys the depending
%  table rather than leaving it with a delay element on a freed table.

test(abolish_destroys_depending_table) :-
    abolish_all_tables,
    retractall(d(_)),
    assertz(d(1)), assertz(d(2)),
    forall(q(_), true),
    forall(r(_), true),
    abolish_table_subgoals(q(_)),
    \+ current_table(r(_), _).

:- table r/1.
r(X) :- q(X).

:- end_tests(abolish_depending_table).
