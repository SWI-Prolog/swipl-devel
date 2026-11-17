/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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

:- module(autoimport_assert_race,
          [ autoimport_assert_race/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).

/** <module> Assert into a module while another thread auto-imports

Thread A asserts M:aiar(1) for a new module M while thread B makes
aiar/1 defined in M, which auto-imports user:aiar/1.  autoImport()
checked that the procedure was undefined without the lock that
assert/1 uses to make it dynamic.  It could thus import user:aiar/1
after assert/1 made the local predicate dynamic, after which assert/1
added its clause to the abandoned local definition (lost) or to the
static user:aiar/1.  The race is narrow, so this test rarely fails
without the fix.  It verifies that each round has one of the two
legal outcomes.
*/

autoimport_assert_race :-
    run_tests([ autoimport_assert_race
              ]).

:- begin_tests(autoimport_assert_race,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(race, Bad-UserClauses == []-1) :-
    race(2000, Bad),
    aggregate_all(count, clause(user:aiar(_), true), UserClauses).

:- end_tests(autoimport_assert_race).

user:aiar(u).                           % static, in user

:- dynamic
    assert_result/2.

race(N, Bad) :-
    retractall(assert_result(_,_)),
    flag(aiar_arrived, _, 0),
    thread_create(asserter(N), A, []),
    thread_create(definer(N), B, []),
    thread_join(A, SA),
    thread_join(B, SB),
    assertion(SA-SB == true-true),
    findall(I-R, (between(1, N, I), result(I, R), R \== ok), Bad).

module_name(I, M) :-
    atom_concat(aiar_m_, I, M).

asserter(N) :-
    big_body(Body),
    forall(between(1, N, I),
           ( module_name(I, M),
             barrier(I),
             spin(20),
             catch(assertz(M:(aiar(1) :- Body)), E, true),
             (   var(E)
             ->  assertz(assert_result(I, ok))
             ;   assertz(assert_result(I, E))
             ) )).

definer(N) :-
    forall(between(1, N, I),
           ( module_name(I, M),
             barrier(I),
             spin(400),
             catch(ignore('$define_predicate'(M:aiar(_))), _, true) )).

%   result(+Round, -Result)
%
%   Result is `ok` if the round had a legal outcome: the assert
%   succeeded and M has the clause, or the assert raised a permission
%   error and M imports user:aiar/1.

result(I, Result) :-
    module_name(I, M),
    assert_result(I, R),
    (   R == ok
    ->  (   clause(M:aiar(1), _)
        ->  Result = ok
        ;   Result = lost_clause
        )
    ;   R = error(permission_error(_,_,_),_),
        predicate_property(M:aiar(_), imported_from(user))
    ->  Result = ok
    ;   Result = R
    ).

%   barrier(+Round)
%
%   Let both threads start round Round at the same time.

barrier(I) :-
    flag(aiar_arrived, V0, V0+1),
    Need is 2*I,
    (   V0+1 >= Need
    ->  true
    ;   repeat,
        flag(aiar_arrived, V, V),
        V >= Need,
        !
    ).

%   spin(+Max)
%
%   Busy wait a random time.

spin(Max) :-
    random_between(0, Max, K),
    forall(between(1, K, _), true).

%   big_body(-Body)
%
%   A clause body that takes some time to compile, widening the window
%   between creating the procedure and making it dynamic.

big_body(Body) :-
    numlist(1, 200, L),
    foldl(conj, L, true, Body).

conj(X, B0, (B0, X == X)).
