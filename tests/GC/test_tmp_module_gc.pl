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

:- module(test_tmp_module_gc,
          [ test_tmp_module_gc/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(modules)).
:- use_module(library(lists)).
:- if(exists_source(library(thread))).
:- use_module(library(thread)).
:- endif.
:- use_module(library(yall)).
:- use_module(library(aggregate)).

/** <module> Test reclaiming temporary modules

in_temporary_module/3 destroys the module using '$destroy_module'/1,
which abolishes the module and leaves reclaiming the module and its
predicates to procedure and module GC.  See destroyTemporaryModule() in
pl-modul.c.  As test_module_gc.pl, we stop the gc thread, replace the
hazards of this thread before collecting and create module names at
runtime.
*/

test_tmp_module_gc :-
    run_tests([ tmp_module_gc,
                gc_code
              ]).

:- begin_tests(tmp_module_gc,
               [ setup(gc_setup(Old)),
                 cleanup(gc_cleanup(Old))
               ]).

test(load) :-
    program(Text),
    loop(500, load_text(Text), [M]>>call(M:run(_))).
%   Each module is loaded from a new source file.  Atom GC does not
%   reclaim the names of the most recent ones, so we use a loose bound.
test(source_files, true(Files < 100)) :-
    program(Text),
    forall(between(1, 200, _),
           in_temporary_module(M, load_program(M, Text), M:run(_))),
    other_meta_call,
    collect(_, _),
    garbage_collect_atoms,
    aggregate_all(count,
                  ( current_atom(A),
                    sub_atom(A, 0, _, _, 'tmg_file_tmp-')
                  ),
                  Files).
test(assert) :-
    loop(2000, [M]>>(assertz(M:a(1)), assertz(M:(b(X) :- a(X)))),
         [M]>>call(M:b(_))).
test(reuse_name, X-Y == 1-2) :-
    fresh(tmg_reuse, M),
    in_temporary_module(M, assertz(M:p(1)), M:p(X)),
    in_temporary_module(M, assertz(M:p(2)), M:p(Y)).
test(clause_ref, Body == between(1, 3, X)) :-
    fresh(tmg_cref, M),
    program(Text),
    in_temporary_module(M, load_program(M, Text),
                        clause(M:q1(X), Body, Ref)),
    other_meta_call,
    collect(_, _),
    assertion(clause_property(Ref, erased)),
    assertion(\+ clause(_, _, Ref)).
test(hook, error(existence_error(procedure, _))) :-
    fresh(tmg_hook, M),
    in_temporary_module(M, load_program(M, "user:tmg_hook(X) :- helper(X).\n\c
                                             helper(1).\n"),
                        true),
    other_meta_call,
    collect(_, _),
    call(user:tmg_hook(_)).
test(hook_cleanup) :-
    abolish(user:tmg_hook/1).
% A local definition that overrides an import must not release the
% definition of the imported predicate.  This freed lists:subtract/3.
test(overrule_import, L == [1]) :-
    forall(between(1, 20, _),
           in_temporary_module(
               M,
               load_program(M, ":- use_module(library(lists)).\n\c
                                subtract(a, b, c).\n"),
               M:subtract(a, b, c))),
    other_meta_call,
    collect(_, _),
    lists:subtract([1,2], [2], L).
test(threads, [M1-P1 == ok-ok, condition(current_prolog_flag(threads, true))]) :-
    program(Text),
    collect(M0, P0),
    thread_create(gc_loop, GC),
    concurrent_forall(between(1, 8, _),
                      forall(between(1, 50, _),
                             in_temporary_module(M, load_program(M, Text),
                                                 M:run(_))),
                      [threads(4)]),
    thread_signal(GC, throw(stop)),
    thread_join(GC, _),
    other_meta_call,
    collect(M2, P2),
    bounded(M2, M0+2, M1),
    bounded(P2, P0+20, P1).

:- end_tests(tmp_module_gc).

:- begin_tests(gc_code,
               [ setup(gc_setup(Old)),
                 cleanup(gc_cleanup(Old))
               ]).

:- dynamic gc_code_fact/1.

test(domain, error(domain_error(gc_code, foo))) :-
    set_prolog_flag(gc_code, foo).
test(no_clause_gc, S1-S2 == S0-S) :-
    predicate_property(gc_code_fact(_), size(S)),
    setup_call_cleanup(
        set_prolog_flag(gc_code, false),
        ( assertz(gc_code_fact(1)),
          predicate_property(gc_code_fact(_), size(S0)),
          retract(gc_code_fact(1)),
          garbage_collect_clauses,
          predicate_property(gc_code_fact(_), size(S1))
        ),
        set_prolog_flag(gc_code, module)),
    gc_to_size(gc_code_fact(_), S, 100, S2).
test(clause_level, X-Y == 1-2) :-       % destroy only abolishes the module
    fresh(tmg_clause, M),
    setup_call_cleanup(
        set_prolog_flag(gc_code, clause),
        ( in_temporary_module(M, assertz(M:p(1)), M:p(X)),
          in_temporary_module(M, assertz(M:p(2)), M:p(Y))
        ),
        set_prolog_flag(gc_code, module)).

%   gc_to_size(+Head, +Expected, +Tries, -Size)
%
%   Run clause GC until the size of Head is Expected or we tried Tries
%   times.  garbage_collect_clauses/0 does nothing if another thread is
%   running clause GC.  Without a gc thread, any thread may do so when
%   it receives the clause GC signal, and that GC may have started
%   before our clause was erased.

gc_to_size(Head, Expected, Tries, Size) :-
    garbage_collect_clauses,
    predicate_property(Head, size(Size0)),
    (   ( Size0 == Expected ; Tries =< 1 )
    ->  Size = Size0
    ;   sleep(0.01),
        Tries1 is Tries - 1,
        gc_to_size(Head, Expected, Tries1, Size)
    ).

:- end_tests(gc_code).

%   loop(+Count, :Setup, :Goal)
%
%   Run in_temporary_module/3 Count times and verify the number of
%   modules and predicates is bounded.

:- meta_predicate loop(+, 1, 1).

loop(Count, Setup, Goal) :-
    collect(M0, P0),
    forall(between(1, Count, _),
           in_temporary_module(M, call(Setup, M), call(Goal, M))),
    other_meta_call,
    collect(M1, P1),
    assertion(M1 =< M0+2),
    assertion(P1 =< P0+20).

program(Text) :-
    findall(Line, prog_line(Line), Lines),
    atomic_list_concat(Lines, '\n', Text).

prog_line(':- use_module(library(lists)).').
prog_line(':- dynamic counter/1.').
prog_line('counter(0).').
prog_line(Line) :-
    between(1, 5, P),
    between(1, 3, C),
    format(atom(Line), 'p~w(~w, X) :- q~w(X), X > ~w.', [P, C, P, C]).
prog_line(Line) :-
    between(1, 5, P),
    format(atom(Line), 'q~w(X) :- between(1, 3, X).', [P]).
prog_line('run(S) :- findall(X, (member(F,[p1,p2]), call(F,_,X)), L), \c
           sum_list(L, S), retract(counter(C0)), C is C0+1, \c
           assertz(counter(C)), last(L, _).').

load_program(Module, Text) :-
    atom_concat(tmg_file_, Module, File),
    setup_call_cleanup(
        open_string(Text, In),
        load_files(Module:File, [stream(In), silent(true)]),
        close(In)).

load_text(Text, Module) :-
    load_program(Module, Text).

bounded(Value, Max, ok) :-
    Value =< Max,
    !.
bounded(Value, _, Value).

%   fresh(+Prefix, -Module)
%
%   Create a module name at runtime.  Module names that appear in the
%   source are pinned by goal expansion.

fresh(Prefix, Module) :-
    atom_concat(Prefix, '_', Module).

collect(Modules, Predicates) :-
    garbage_collect_clauses,
    garbage_collect_clauses,
    garbage_collect_clauses,
    statistics(modules, Modules),
    statistics(predicates, Predicates).

%   other_meta_call
%
%   Replace the module and procedure hazards of this thread.  See
%   test_module_gc.pl.

other_meta_call :-
    fresh(tmg_other, M),
    catch(call(M:tmg_undefined), error(existence_error(_,_),_), true).

gc_loop :-
    catch(gc_loop_, stop, true).

gc_loop_ :-
    garbage_collect_clauses,
    gc_loop_.

%   Procedure and module GC require the flag gc_code to be `module`.

gc_setup(GCThread-GCCode) :-
    current_prolog_flag(gc_thread, GCThread),
    current_prolog_flag(gc_code, GCCode),
    set_prolog_gc_thread(false),
    set_prolog_flag(gc_code, module).

gc_cleanup(GCThread-GCCode) :-
    set_prolog_flag(gc_code, GCCode),
    set_prolog_gc_thread(GCThread).
