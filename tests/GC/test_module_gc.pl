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

:- module(test_module_gc,
          [ test_module_gc/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- if(exists_source(library(thread))).
:- use_module(library(thread)).
:- endif.
:- use_module(library(yall)).

/** <module> Test garbage collection of modules

Calling M:G or asserting into M for a new module M creates the module.
Module GC reclaims modules that are empty and not referenced as part of
clause GC.  See "Module garbage collection" in pl-modul.c.  We stop the
gc thread as garbage_collect_clauses/0 returns immediately if the gc
thread is running clause GC.

Each thread keeps the module of its last meta-call in a hazard pointer.
Tests that check a module is collected therefore make a meta-call to
another module first.  Module names that appear in the source are pinned
by goal expansion, so these tests create module names at runtime.
*/

test_module_gc :-
    run_tests([ module_gc
              ]).

:- begin_tests(module_gc,
               [ setup(gc_setup(Old)),
                 cleanup(gc_cleanup(Old))
               ]).

test(meta_call) :-
    loop(mgc_mc_, [M]>>catch(call(M:foo), error(existence_error(_,_),_),
                             true)).
test(abolish_module) :-
    loop(mgc_am_, [M]>>( assertz(M:p(1)),
                         assertz(M:(q(X) :- p(X))),
                         call(M:q(_)),
                         abolish_module(M)
                       )).
test(retract) :-
    loop(mgc_rt_, [M]>>(assertz(M:p(1)), retract(M:p(_)), abolish_module(M))).
test(retractall) :-
    loop(mgc_ra_, [M]>>(assertz(M:p(1)), retractall(M:p(_)),
                        abolish_module(M))).
test(abolish) :-
    loop(mgc_ab_, [M]>>(assertz(M:p(1)), abolish(M:p/1))).
test(clause) :-
    loop(mgc_cl_, [M]>>(assertz(M:p(1)), clause(M:p(_), true),
                        abolish_module(M))).
test(dynamic) :-
    loop(mgc_dy_, [M]>>(dynamic(M:p/1), abolish_module(M))).
test(predicate_property) :-
    loop(mgc_pp_, [M]>>(assertz(M:p(1)), predicate_property(M:p(_), dynamic),
                        abolish_module(M))).
test(current_predicate) :-
    loop(mgc_cp_, [M]>>(assertz(M:p(1)), current_predicate(M:p/1),
                        abolish_module(M))).
test(collected, fail) :-
    fresh(mgc_c, M),
    catch(call(M:foo), error(existence_error(_,_),_), true),
    other_meta_call,
    collect(_),
    current_module(M).
test(recreate, X == 2) :-
    fresh(mgc_rc, M),
    assertz(M:p(1)),
    abolish_module(M),
    other_meta_call,
    collect(_),
    \+ current_module(M),
    assertz(M:p(2)),
    call(M:p(X)).
test(pinned) :-
    fresh(mgc_pin, M),
    set_prolog_flag(M:unknown, fail),
    abolish_module(M),
    other_meta_call,
    collect(_),
    current_module(M).
test(compiled_ref, true) :-
    assertz(mgc_cr_caller :- mgc_cr:p),
    assertz(mgc_cr:p),
    abolish_module(mgc_cr),
    other_meta_call,
    collect(_),
    assertz(mgc_cr:p),
    mgc_cr_caller.
test(running, X == done) :-
    assertz(mgc_run:(r(X) :-
                       abolish_module(mgc_run),
                       test_module_gc:other_meta_call,
                       garbage_collect_clauses,
                       X = done)),
    call(mgc_run:r(X)).
test(context, M == mgc_ctx) :-
    call(mgc_ctx:context_module(M)).
test(context_collected, fail) :-
    fresh(mgc_ctx2, M),
    call(M:context_module(_)),
    other_meta_call,
    collect(_),
    current_module(M).
test(threads, [true, condition(current_prolog_flag(threads, true))]) :-
    numlist(1, 50, L),
    maplist([I,M]>>atom_concat(mgc_t_, I, M), L, Pool),
    thread_create(gc_loop, GC),
    concurrent_forall(between(1, 8, T),
                      module_pool(T, Pool),
                      [threads(4)]),
    thread_signal(GC, throw(stop)),
    thread_join(GC, _),
    forall(member(M, Pool),
           ( abolish_module(M),
             assertz(M:mgc_tp(x)),
             once(call(M:mgc_tp(X))),
             assertion(X == x)
           )).

:- end_tests(module_gc).

%   loop(+Prefix, :Goal)
%
%   Call Goal on 2000 new modules and verify the number of modules is
%   bounded.

loop(Prefix, Goal) :-
    collect(M0),
    forall(between(1, 2000, I),
           ( atom_concat(Prefix, I, M),
             call(Goal, M)
           )),
    other_meta_call,
    collect(M1),
    assertion(M1 < M0+100).

%   fresh(+Prefix, -Module)
%
%   Create a module name at runtime.  Module names that appear in the
%   source are pinned by goal expansion.

fresh(Prefix, Module) :-
    atom_concat(Prefix, '_', Module).

collect(Modules) :-
    garbage_collect_clauses,
    garbage_collect_clauses,
    statistics(modules, Modules).

%   other_meta_call
%
%   Replace the module and procedure hazards of this thread.  This
%   requires a meta-call to an undefined predicate in a module that is
%   not pinned.  Note that undefined/0 is a system predicate.

other_meta_call :-
    fresh(mgc_other, M),
    catch(call(M:mgc_undefined), error(existence_error(_,_),_), true).

module_pool(T, Pool) :-
    forall(between(1, 2000, _),
           ( random_member(M, Pool),
             random_between(1, 9, Op),
             module_op(Op, M, T)
           )).

module_op(1, M, T) :- assertz(M:mgc_tp(T)).
module_op(2, M, _) :- catch(ignore(call(M:mgc_tp(_))),
                            error(existence_error(_,_),_), true).
module_op(3, M, _) :- ignore(retract(M:mgc_tp(_))).
module_op(4, M, _) :- abolish_module(M).
module_op(5, M, _) :- abolish(M:mgc_tp/1).
module_op(6, M, _) :- ignore(current_predicate(M:mgc_tp/1)).
module_op(7, M, _) :- ignore(predicate_property(M:mgc_tp(_), dynamic)).
module_op(8, M, _) :- ignore(clause(M:mgc_tp(_), _)).
module_op(9, M, _) :- catch(call(M:mgc_undefined),
                            error(existence_error(_,_),_), true).

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
