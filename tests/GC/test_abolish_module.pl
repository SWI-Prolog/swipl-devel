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

:- module(test_abolish_module,
          [ test_abolish_module/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- if(exists_source(library(thread))).
:- use_module(library(thread)).
:- endif.
:- use_module(library(yall)).

/** <module> Test abolish_module/1

abolish_module/1 resets a module to a new empty module.  Clauses are
reclaimed by clause GC and unreferenced procedures by procedure GC.  We
stop the gc thread as garbage_collect_clauses/0 returns immediately if
the gc thread is running clause GC.
*/

test_abolish_module :-
    run_tests([ abolish_module
              ]).

:- begin_tests(abolish_module,
               [ setup(gc_setup(Old)),
                 cleanup(gc_cleanup(Old))
               ]).

test(predicates, Ps == []) :-
    assertz(am_p:p(1)),
    assertz(am_p:(q(X) :- p(X))),
    abolish_module(am_p),
    findall(P, (current_predicate(am_p:P), P \= '$'/_), Ps).
test(reclaim) :-
    collect(P0),
    forall(between(1, 5000, I),
           ( atom_concat(am_r_, I, M),
             assertz(M:p(I)),
             assertz(M:q(I)),
             abolish_module(M)
           )),
    collect(P1),
    assertion(P1 < P0+100).
test(exports, E == []) :-
    am_e:export(am_e:p/0),
    abolish_module(am_e),
    module_property(am_e, exports(E)).
test(operators, Ops == []) :-
    op(700, xfx, am_o:(===>)),
    abolish_module(am_o),
    findall(op(P,T), current_op(P, T, am_o:(===>)), Ops).
test(import_modules, Ms == [user]) :-
    add_import_module(am_i, lists, end),
    abolish_module(am_i),
    findall(M, import_module(am_i, M), Ms).
test(class, C == user) :-
    set_module(am_c:class(library)),
    abolish_module(am_c),
    module_property(am_c, class(C)).
test(compiled_caller, true) :-
    assertz(am_cc:p),
    assertz(user:(am_cc_caller :- am_cc:p)),
    abolish_module(am_cc),
    assertz(am_cc:p),
    user:am_cc_caller,
    retractall(user:am_cc_caller).
test(importer, error(existence_error(procedure, am_x:p/0), _)) :-
    assertz(am_x:p),
    am_x:export(am_x:p/0),
    am_y:import(am_x:p/0),
    abolish_module(am_x),
    am_y:p.
test(running, X == done) :-
    assertz((am_run:r(X) :- abolish_module(am_run), am_run_cont(X))),
    am_run:r(X).
test(nonexisting, fail) :-
    abolish_module(am_none),
    current_module(am_none).
test(thread_local_abolish,
     error(permission_error(modify, thread_local_procedure, _), _)) :-
    thread_local(am_tla:p/1),
    abolish(am_tla:p/1).
%   The module name is written in this file, so the module is pinned.
%   Abolishing it drops the thread-local predicate and the import, and
%   keeps the module.  The names are not predicates of user: this file
%   is loaded by tests/test.pl, which defines user:p/1 and imports
%   library(lists), and current_predicate/1 walks the super chain.
test(thread_local) :-
    set_prolog_flag(am_tl:unknown, fail),
    assertz(am_src:am_only_here(ok)),
    am_src:export(am_src:am_only_here/1),
    thread_local(am_tl:am_tl_p/1),
    assertz(am_tl:am_tl_p(from_here)),
    am_tl:import(am_src:am_only_here/1),
    assertion(am_tl:am_only_here(ok)),
    assertion(am_tl:am_tl_p(from_here)),
    abolish_module(am_tl),
    assertion(\+ current_predicate(am_tl:am_tl_p/1)),
    assertion(\+ predicate_property(am_tl:am_tl_p(_), thread_local)),
    assertion(\+ current_predicate(am_tl:am_only_here/1)),
    assertion(catch(am_tl:am_only_here(_),
                    error(existence_error(procedure, _), _),
                    true)),
    assertion(current_module(am_tl)),
    assertion(am_src:am_only_here(ok)).
test(thread_local_remote,
     condition(current_prolog_flag(threads, true))) :-
    thread_local(am_tlr:am_tlr_p/1),
    thread_create(assertz(am_tlr:am_tlr_p(remote)), Id),
    thread_join(Id, true),
    abolish_module(am_tlr),
    assertion(\+ current_predicate(am_tlr:am_tlr_p/1)).
test(user, error(permission_error(abolish, module, user), _)) :-
    abolish_module(user).
test(system, error(permission_error(abolish, module, system), _)) :-
    abolish_module(system).
test(file, X == 42) :-
    tmp_file_stream(text, File, Out),
    format(Out, ':- module(am_file, [f/1]).~n', []),
    format(Out, 'f(X) :- g(X).~n', []),
    format(Out, 'g(42).~n', []),
    format(Out, 'user:am_file_hook.~n', []),
    close(Out),
    am_load:use_module(File),
    am_load:f(_),
    abolish_module(am_file),
    assertion(\+ source_file(File)),
    assertion(\+ catch(user:am_file_hook, _, fail)),
    catch(am_load:f(_), error(existence_error(procedure, _), _), true),
    am_load:use_module(File),
    am_load:f(X),
    delete_file(File).
test(threads, condition(current_prolog_flag(threads, true))) :-
    numlist(1, 20, L),
    maplist([I,M]>>atom_concat(am_t_, I, M), L, Pool),
    thread_create(gc_loop, GC),
    concurrent_forall(between(1, 8, _),
                      forall(between(1, 2000, _),
                             ( random_member(M, Pool),
                               random_between(1, 4, Op),
                               thread_op(Op, M)
                             )),
                      [threads(4)]),
    thread_signal(GC, throw(stop)),
    thread_join(GC, _),
    forall(member(M, Pool),
           ( abolish_module(M),
             assertz(M:am_tp(1)),
             M:am_tp(X),
             assertion(X == 1)
           )).

:- end_tests(abolish_module).

am_run_cont(done).

thread_op(1, M) :- assertz(M:am_tp(1)).
thread_op(2, M) :- abolish_module(M).
thread_op(3, M) :- catch(ignore(M:am_tp(_)),
                         error(existence_error(_,_),_), true).
thread_op(4, M) :- ignore(retract(M:am_tp(_))).

collect(Predicates) :-
    garbage_collect_clauses,
    garbage_collect_clauses,
    statistics(predicates, Predicates).

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
