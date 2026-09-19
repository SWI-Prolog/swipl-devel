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


:- module(test_reeval_wfs,
          [ test_reeval_wfs/0
          ]).
:- use_module(library(plunit)).

/** <module> Well founded semantics after incremental re-evaluation

Re-evaluating an incremental table must  give the same truth values as
evaluating  the same  program from  scratch.  Preparing  a table  for
re-evaluation removes the  delay lists of its conditional  answers; if
the  count of  undefined answers  is not  updated with  them, the
table  keeps claiming  undefined answers  it no  longer has  and
propagate_to_answer() can  never decide  a negative  delay on  it.  An
answer  of another  table  delaying on  tnot(Table) then  stays
conditional although Table was re-evaluated to definitely false.
*/

test_reeval_wfs :-
    run_tests([ reeval_wfs
              ]).

:- begin_tests(reeval_wfs).

:- dynamic d/1 as incremental.
:- dynamic blk/1 as incremental.
:- table q/1 as incremental.
:- table u/1 as incremental.

%  q(X) and u(X) are a negative loop, so both are undefined -- unless
%  blk(X) makes u(X) definitely false, which makes q(X) definitely true.

q(X) :- d(X), tnot(u(X)).
u(X) :- d(X), tnot(q(X)), \+ blk(X).

goals([q(1), q(2), q(3), u(1), u(2), u(3)]).

truth(Goal, Truth) :-
    (   call_delays(Goal, Delays)
    ->  (   Delays == true
        ->  Truth = true
        ;   Truth = undefined
        )
    ;   Truth = false
    ).

truths(Truths) :-
    goals(Goals),
    findall(G-T, (member(G, Goals), truth(G, T)), Truths).

%!  incremental_vs_fresh(+Updates, -Incremental, -Fresh)
%
%   Query the program, apply Updates, query again  and collect the truth
%   values.  Then discard all tables  and collect them again, now from a
%   clean slate.

incremental_vs_fresh(Updates, Incremental, Fresh) :-
    abolish_all_tables,
    retractall(d(_)), retractall(blk(_)),
    forall(member(X, [1,2,3]), assertz(d(X))),
    forall(q(_), true),
    forall(member(U, Updates), assertz(U)),
    forall(q(_), true),
    truths(Incremental),
    abolish_all_tables,
    forall(q(_), true),
    truths(Fresh).

test(one_table_becomes_false, Incremental == Fresh) :-
    incremental_vs_fresh([blk(1)], Incremental, Fresh).
test(two_tables_become_false, Incremental == Fresh) :-
    incremental_vs_fresh([blk(1), blk(3)], Incremental, Fresh).
test(all_tables_become_false, Incremental == Fresh) :-
    incremental_vs_fresh([blk(1), blk(2), blk(3)], Incremental, Fresh).

%  ... and the values themselves: the blocked ones are true, the others
%  stay undefined because their negative loop is intact.

test(truth_values, Truths == [ q(1)-true,      q(2)-undefined, q(3)-true,
                               u(1)-false,     u(2)-undefined, u(3)-false
                             ]) :-
    incremental_vs_fresh([blk(1), blk(3)], Truths, _).

:- end_tests(reeval_wfs).
