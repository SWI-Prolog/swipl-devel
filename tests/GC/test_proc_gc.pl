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

:- module(test_proc_gc,
          [ test_proc_gc/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- if(exists_source(library(thread))).
:- use_module(library(thread)).
:- endif.

/** <module> Test garbage collection of procedures

Calling an undefined predicate using call/N creates a stub procedure.
Such stubs are collected by clause garbage collection if they are not
referenced.  See "PROCEDURE GC" in pl-proc.c.

garbage_collect_clauses/0 returns immediately if another thread, notably
the gc thread, is running clause GC, which includes procedure GC.  We
stop the gc thread during these tests.
*/

test_proc_gc :-
    run_tests([ proc_gc
              ]).

:- meta_predicate
    in_module(:).

:- begin_tests(proc_gc,
                [ setup(gc_setup(Old)),
                  cleanup(gc_cleanup(Old))
                ]).

test(fail) :-
    collect(P0),
    forall(between(1, 20000, I),
           ( atom_concat(pgc_f_, I, N),
             \+ in_module(pgc_fail:N)
           )),
    collect(P1),
    assertion(P1 < P0+100).
test(error) :-
    collect(P0),
    forall(between(1, 20000, I),
           ( atom_concat(pgc_e_, I, N),
             G =.. [N, x],
             catch(call(G), error(existence_error(procedure, _), _), true)
           )),
    collect(P1),
    assertion(P1 < P0+100).
test(functors) :-
    collect(_),
    garbage_collect_atoms,
    statistics(functors, F0),
    forall(between(1, 20000, I),
           ( atom_concat(pgc_t_, I, N),
             G =.. [N, x],
             \+ in_module(pgc_fail:G)
           )),
    collect(_),
    garbage_collect_atoms,
    statistics(functors, F1),
    assertion(F1 < F0+1000).
test(define_after, true) :-
    \+ in_module(pgc_fail:pgc_da),
    collect(_),
    assertz(pgc_fail:pgc_da),
    in_module(pgc_fail:pgc_da).
test(define_stub, true) :-
    \+ in_module(pgc_fail:pgc_ds),
    assertz(pgc_fail:pgc_ds),
    collect(_),
    in_module(pgc_fail:pgc_ds).
test(dynamic_stub, true) :-
    \+ in_module(pgc_fail:pgc_dyn),
    dynamic(pgc_fail:pgc_dyn/0),
    collect(_),
    predicate_property(pgc_fail:pgc_dyn, dynamic).
test(compiled_ref, true) :-
    \+ in_module(pgc_fail:pgc_cr),
    assertz(pgc_fail:(pgc_cr_caller :- pgc_cr)),
    collect(_),
    assertz(pgc_fail:pgc_cr),
    in_module(pgc_fail:pgc_cr_caller).
test(clause_ref) :-
    collect(P0),
    forall(between(1, 5000, I),
           ( atom_concat(pgc_cr_, I, N),
             assertz(pgc_fail:(pgc_crc(I) :- N)),
             retract(pgc_fail:(pgc_crc(I) :- _))
           )),
    collect(P1),
    assertion(P1 < P0+100).
test(clause_ref_kept, true) :-
    assertz(pgc_fail:(pgc_crk :- pgc_crk_undef)),
    collect(_),
    predicate_property(pgc_fail:pgc_crk_undef, undefined),
    \+ in_module(pgc_fail:pgc_crk),
    assertz(pgc_fail:pgc_crk_undef),
    in_module(pgc_fail:pgc_crk).
test(clause_ref_error, error(existence_error(procedure, _))) :-
    assertz((pgc_cre :- pgc_cre_undef)),
    collect(_),
    pgc_cre.
test(no_current, fail) :-
    \+ in_module(pgc_fail:pgc_nc),
    current_predicate(pgc_fail:pgc_nc/0).
test(threads, [true, condition(current_prolog_flag(threads, true))]) :-
    Names = 200,
    numlist(1, Names, L),
    maplist([I,N]>>atom_concat(pgc_mt_, I, N), L, Pool),
    thread_create(gc_loop, GC),
    concurrent_forall(between(1, 8, T),
                      call_pool(T, Pool),
                      [threads(4)]),
    thread_signal(GC, throw(stop)),
    thread_join(GC, _),
    forall(( member(N, Pool),
             predicate_property(pgc_fail:N, defined)
           ),
           in_module(pgc_fail:N)).
test(abolish) :-
    collect(P0),
    forall(between(1, 5000, I),
           ( atom_concat(pgc_ab_, I, N),
             G =.. [N, I],
             assertz(pgc_ab:G),
             in_module(pgc_ab:G),
             abolish(pgc_ab:N/1)
           )),
    collect(P1),
    assertion(P1 < P0+100).
test(abolish_dynamic) :-
    collect(P0),
    forall(between(1, 5000, I),
           ( atom_concat(pgc_abd_, I, N),
             dynamic(pgc_ab:N/1),
             G =.. [N, I],
             assertz(pgc_ab:G),
             retract(pgc_ab:G),
             abolish(pgc_ab:N/1)
           )),
    collect(P1),
    assertion(P1 < P0+100).
test(abolish_reassert, X == 2) :-
    assertz(pgc_ab:pgc_abr(1)),
    abolish(pgc_ab:pgc_abr/1),
    collect(_),
    \+ current_predicate(pgc_ab:pgc_abr/1),
    assertz(pgc_ab:pgc_abr(2)),
    pgc_ab:pgc_abr(X).
test(abolish_running, X == done) :-
    assertz(pgc_ab:(pgc_abrun(X) :-
                       abolish(pgc_ab:pgc_abrun/1),
                       garbage_collect_clauses,
                       X = done)),
    pgc_ab:pgc_abrun(X).
test(abolish_call, error(existence_error(procedure, _))) :-
    assertz(pgc_ab:pgc_abc),
    in_module(pgc_ab:pgc_abc),
    abolish(pgc_ab:pgc_abc/0),
    collect(_),
    in_module(pgc_ab:pgc_abc).
test(abolish_clause_ref, PI == pgc_ab:pgc_abcr/1) :-
    assertz(pgc_ab:pgc_abcr(1), Ref),
    abolish(pgc_ab:pgc_abcr/0),
    abolish(pgc_ab:pgc_abcr/1),
    collect(_),
    collect(_),
    clause_property(Ref, erased),
    clause_property(Ref, predicate(PI)),
    \+ clause(_, _, Ref).
test(abolish_threads, [true, condition(current_prolog_flag(threads, true))]) :-
    Names = 100,
    numlist(1, Names, L),
    maplist([I,N]>>atom_concat(pgc_abt_, I, N), L, Pool),
    thread_create(gc_loop, GC),
    concurrent_forall(between(1, 8, T),
                      abolish_pool(T, Pool),
                      [threads(4)]),
    thread_signal(GC, throw(stop)),
    thread_join(GC, _),
    forall(member(N, Pool),
           ( abolish(pgc_abt:N/1),
             G =.. [N, X],
             assertz(pgc_abt:G),
             once(in_module(pgc_abt:G)),
             assertion(var(X))
           )).

:- end_tests(proc_gc).

in_module(M:G) :-
    call(M:G).

collect(Predicates) :-
    garbage_collect_clauses,
    statistics(predicates, Predicates).

call_pool(T, Pool) :-
    forall(between(1, 5000, I),
           ( random_member(N, Pool),
             (   T == 1, I mod 50 =:= 0
             ->  assertz(pgc_fail:N)
             ;   ignore(in_module(pgc_fail:N))
             )
           )).

abolish_pool(T, Pool) :-
    forall(between(1, 5000, I),
           ( random_member(N, Pool),
             G =.. [N, T],
             random_between(1, 7, Op),
             abolish_op(Op, G, I)
           )).

abolish_op(1, G, _) :- assertz(pgc_abt:G).
abolish_op(2, G, _) :- ignore(in_module(pgc_abt:G)).
abolish_op(3, G, _) :- functor(G, N, A), abolish(pgc_abt:N/A).
abolish_op(4, G, _) :- ignore(retract(pgc_abt:G)).
abolish_op(5, G, _) :- functor(G, N, A), ignore(current_predicate(pgc_abt:N/A)).
abolish_op(6, G, _) :- ignore(clause(pgc_abt:G, _)).
abolish_op(7, G, _) :-
    functor(G, N, A),
    (   catch(dynamic(pgc_abt:N/A), error(permission_error(_,_,_),_), true)
    ->  true
    ;   true
    ).

gc_loop :-
    catch(gc_loop_, stop, true).

gc_loop_ :-
    garbage_collect_clauses,
    gc_loop_.

:- set_prolog_flag(pgc_fail:unknown, fail).
:- set_prolog_flag(pgc_abt:unknown, fail).

%   Procedure and module GC require the flag gc_code to be `module`.

gc_setup(GCThread-GCCode) :-
    current_prolog_flag(gc_thread, GCThread),
    current_prolog_flag(gc_code, GCCode),
    set_prolog_gc_thread(false),
    set_prolog_flag(gc_code, module).

gc_cleanup(GCThread-GCCode) :-
    set_prolog_flag(gc_code, GCCode),
    set_prolog_gc_thread(GCThread).
