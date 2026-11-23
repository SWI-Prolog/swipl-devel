/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2021-2025, University of Amsterdam
                              VU University Amsterdam
		              CWI, Amsterdam
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

:- module(test_undo,
          [ test_undo/0
          ]).
:- use_module(library(plunit)).

test_undo :-
    run_tests([ undo
              ]).

:- begin_tests(undo).

:- dynamic a/1.

test(undo_or) :-
    (   assert(a(1), Ref),
        undo(erase(Ref)),
        fail
    ;   \+ a(1)
    ).
test(undo_or_other_clause) :-          % a/1 is not empty after the undo
    assert(a(0), Ref0),
    (   assert(a(1), Ref),
        undo(erase(Ref)),
        fail
    ;   \+ a(1)
    ),
    erase(Ref0).
test(clauses) :-
    t1.
test(error, throws(x)) :-
    (   undo(throw(x)),
        fail
    ;   ok                              % true is a VMI
    ).
test(error2) :-
    catch((assert(a(3), Ref),
           undo(erase(Ref)),
           throw(x)),
          x,
          true),
    \+ a(3).
test(cleanup, fail) :-                 % issue #1414
    \+ \+ once((undo(true), fail)).
test(gc, Ran == [first,second]) :-       % GC merged the undo list trail
    nb_setval(undo_ran, []),
    (   undo(undo_ran(first)),
        undo(undo_ran(second)),
        garbage_collect,
        fail
    ;   nb_getval(undo_ran, Ran0),
        msort(Ran0, Ran)
    ).
test(reclaimed, true(Left < 100)) :-   % undo goals were never reclaimed
    forall(between(1, 1000, _), (undo(true), fail ; true)),
    garbage_collect,
    garbage_collect_atoms,
    aggregate_all(count, current_blob(_, undo), Left).
test(local_reclaimed, true(Collected >= 4000)) :- % thread-local blobs
    engine_create(C, undo_churn(5000, C), E),  % fresh sweep threshold
    call_cleanup(engine_next(E, Collected), engine_destroy(E)).

undo_churn(N, Collected) :-
    '$local_atoms'(collected, C0),
    forall(between(1, N, _), (undo(true), fail ; true)),
    '$local_atoms'(collected, C1),
    Collected is C1-C0.

undo_ran(X) :-
    nb_getval(undo_ran, L),
    nb_setval(undo_ran, [X|L]).

ok.

t1 :-
    assert(a(1), Ref),
    undo(erase(Ref)),
    fail.
t1 :-
    \+ a(1).

:- end_tests(undo).
