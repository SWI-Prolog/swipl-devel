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

:- module(test_functor_gc,
          [ test_functor_gc/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Test garbage collection of functors

Functors created from data, e.g., by =../2, functor/3 or read/1, are
collected by atom garbage collection if no term, clause or record uses
them.
*/

test_functor_gc :-
    run_tests([ functor_gc
              ]).

:- begin_tests(functor_gc).

test(collect) :-
    garbage_collect_atoms,
    statistics(functors, F0),
    forall(between(1, 50000, I),
           ( atom_concat(fgc_c_, I, N),
             T =.. [N, x],
             arg(1, T, x)
           )),
    collect_functors(F0+1000, 10),
    statistics(functors, F1),
    assertion(F1 < F0+1000).
test(read, true(F1 < F0+1000)) :-
    garbage_collect_atoms,
    statistics(functors, F0),
    forall(between(1, 20000, I),
           ( format(atom(A), 'fgc_r_~w(x)', [I]),
             term_to_atom(T, A),
             arg(1, T, x)
           )),
    collect_functors(F0+1000, 10),
    statistics(functors, F1).
test(keep) :-
    T1 =.. [fgc_k_clause, a],
    assertz(kept(T1)),
    T2 =.. [fgc_k_record, b],
    recorda(fgc_k, T2, Ref),
    T3 =.. [fgc_k_pred, c],
    assertz(T3),
    T4 =.. [fgc_k_stack, d],
    garbage_collect_atoms,
    garbage_collect_atoms,
    kept(K1), K1 =.. L1, assertion(L1 == [fgc_k_clause, a]),
    recorded(fgc_k, K2), K2 =.. L2, assertion(L2 == [fgc_k_record, b]),
    erase(Ref),
    assertion(call(fgc_k_pred, c)),
    T4 =.. L4, assertion(L4 == [fgc_k_stack, d]),
    retractall(kept(_)).
test(retract, true(F1 < F0+1000)) :-
    garbage_collect_atoms,
    statistics(functors, F0),
    forall(between(1, 20000, I),
           ( atom_concat(fgc_a_, I, N),
             T =.. [N, x],
             assertz(kept(T)),
             retract(kept(T))
           )),
    collect_functors(F0+1000, 10),
    statistics(functors, F1).
test(findall, L == [[fgc_h_findall1, 1], [fgc_h_findall2, 2]]) :-
    findall(T, ( between(1, 2, I),
                 atom_concat(fgc_h_findall, I, N),
                 T =.. [N, I],
                 garbage_collect_atoms
               ), Ts),
    maplist(univ, Ts, L).
test(queue, [ condition(current_prolog_flag(threads, true)),
              L == [fgc_h_queue, 1]
            ]) :-
    message_queue_create(Q),
    \+ \+ ( T =.. [fgc_h_queue, 1],
            thread_send_message(Q, T)
          ),
    clear_volatile,
    garbage_collect_atoms,
    garbage_collect_atoms,
    thread_get_message(Q, T),
    message_queue_destroy(Q),
    T =.. L.
test(clause, L == [fgc_h_clause, 1]) :-
    \+ \+ ( T =.. [fgc_h_clause, 1],
            assertz(kept(T))
          ),
    clear_volatile,
    garbage_collect_atoms,
    garbage_collect_atoms,
    retract(kept(T)),
    T =.. L.
test(record, L == [fgc_h_record, 1]) :-
    \+ \+ ( T =.. [fgc_h_record, 1],
            recorda(fgc_h, T, _)
          ),
    clear_volatile,
    garbage_collect_atoms,
    garbage_collect_atoms,
    recorded(fgc_h, T, Ref),
    erase(Ref),
    T =.. L.
test(global, L == [fgc_h_gvar, 1]) :-
    \+ \+ ( T =.. [fgc_h_gvar, 1],
            nb_setval(fgc_h, T)
          ),
    clear_volatile,
    garbage_collect_atoms,
    garbage_collect_atoms,
    nb_getval(fgc_h, T),
    nb_delete(fgc_h),
    T =.. L.
test(concurrent, condition(current_prolog_flag(threads, true))) :-
    thread_create(agc_loop, AGC, []),
    findall(T, ( between(1, 4, W),
                 thread_create(worker(W, 10000), T, [])
               ), Workers),
    maplist(join, Workers),
    thread_send_message(AGC, done),
    join(AGC).

:- end_tests(functor_gc).

:- dynamic kept/1.

%   Reclaim unused functors.  garbage_collect_atoms/0 does nothing if
%   another thread (the gc thread) is running atom-GC, and retracted
%   clauses may need multiple runs of clause GC.

collect_functors(Max, Tries) :-
    garbage_collect_clauses,
    garbage_collect_atoms,
    statistics(functors, F),
    (   F < Max
    ->  true
    ;   Tries > 0
    ->  sleep(0.1),
        Tries1 is Tries - 1,
        collect_functors(Max, Tries1)
    ;   true
    ).

univ(T, L) :-
    T =.. L.

%   The last functor created by a thread is protected.  Create another
%   one.

clear_volatile :-
    _ =.. [fgc_clear_volatile, x].

join(T) :-
    thread_join(T, Status),
    assertion(Status == true).

agc_loop :-
    thread_self(Me),
    (   thread_get_message(Me, done, [timeout(0)])
    ->  true
    ;   garbage_collect_atoms,
        agc_loop
    ).

worker(W, N) :-
    forall(between(1, N, I),
           work(W, I)).

%   Create a fresh functor and check it survives all operations that
%   copy or temporarily hide it.

work(W, I) :-
    format(atom(Name), 'fgc_w~w_~w', [W, I]),
    T =.. [Name, I, g(Name)],
    check(T, Name, I),
    findall(T, true, [T1]),
    check(T1, Name, I),
    copy_term(T, T2),
    check(T2, Name, I),
    C = c(C, T),
    copy_term(C, C2),
    C2 = c(_, T3),
    check(T3, Name, I),
    term_hash(T, _),
    variant_sha1(T, _),
    term_to_atom(T, A),
    term_to_atom(T4, A),
    check(T4, Name, I),
    thread_self(Me),
    thread_send_message(Me, msg(T)),
    thread_get_message(Me, msg(T5)),
    check(T5, Name, I),
    assertz(kept(T), Ref),
    clause(kept(T6), true, Ref),
    erase(Ref),
    check(T6, Name, I),
    check(T, Name, I).

check(T, Name, I) :-
    (   T =.. [Name, I, g(Name)]
    ->  true
    ;   format(user_error, 'Wrong term: ~q, expected ~q/~w~n', [T, Name, I]),
        fail
    ).
