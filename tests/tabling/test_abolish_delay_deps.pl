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


:- module(test_abolish_delay_deps,
          [ test_abolish_delay_deps/0
          ]).
:- use_module(library(plunit)).

/** <module> Back-references from a table to the answers that delay on it

A conditional answer  that delays on another table's  answer is recorded
twice: in  its own delay  list and in  the `delays` buffer  of the
worklist of the table  it delays on.  The latter is  what lets an
abolish of  that table  find the  tables that  depend on  it.  A
simplification  propagating  one  answer's   truth  value  used  to
remove  the back-reference  of  every  answer in  that  buffer, also
those  delaying on  a different  answer of  the table.   A subsequent
abolish then left them with a delay element into a destroyed table.
*/

test_abolish_delay_deps :-
    run_tests([ abolish_delay_deps
              ]).

:- begin_tests(abolish_delay_deps).

:- dynamic d/1 as incremental.
:- dynamic blk/1 as incremental.
:- table q/1 as incremental.
:- table u/1 as incremental.
:- table p/1.                           % plain: not invalidated by the assert

q(X) :- d(X), tnot(u(X)).
u(X) :- d(X), tnot(q(X)), \+ blk(X).
p(X) :- q(X).

%  Create the state: p/1 has two conditional answers, each delaying on one
%  of q/1's; asserting blk(1) makes q(1) definite on re-evaluation, which
%  propagates and used to drain the back-references of both.

setup_delays :-
    abolish_all_tables,
    retractall(d(_)), retractall(blk(_)),
    assertz(d(1)), assertz(d(2)),
    forall(q(_), true),
    forall(p(_), true),
    assertz(blk(1)),
    forall(q(_), true).

test(abolish_finds_depending_table) :-
    setup_delays,
    abolish_table_subgoals(q(_)),
    \+ current_table(p(_), _).

%  ... and if it is left behind, reading its answers reads a destroyed
%  table.  '$tbl_answer'/3 is what answer completion uses for this.

test(answers_of_depending_table_are_readable) :-
    setup_delays,
    abolish_table_subgoals(q(_)),
    forall(( current_table(p(_), T),
             '$tbl_answer'(T, _Return, Condition)
           ),
           nonvar(Condition)).

%  The simplification itself must still do its work: the answer for 1
%  becomes unconditional, the one for 2 stays conditional.

test(simplification, Conditions == [1-true, 2-conditional]) :-
    setup_delays,
    findall(N-C, answer_condition(q(_), N, C), Conditions0),
    msort(Conditions0, Conditions).

answer_condition(Variant, N, Condition) :-
    current_table(Variant, Trie),
    '$tbl_answer'(Trie, Return, Cond),
    arg(1, Return, N),
    (   Cond == true
    ->  Condition = true
    ;   Condition = conditional
    ).

:- end_tests(abolish_delay_deps).
