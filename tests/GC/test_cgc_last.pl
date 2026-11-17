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

:- module(test_cgc_last,
          [ test_cgc_last/0
          ]).
:- use_module(library(plunit)).

/** <module> Reclaim the last retracted clause

Clause garbage collection did not reclaim a clause that was erased at
the generation at which it started, i.e., the last database update
before garbage_collect_clauses/0.  This clause was only reclaimed after
some other database update.

garbage_collect_clauses/0 returns immediately if another thread, notably
the gc thread, is running clause GC.  That run may have started before
our clause was erased, so we stop the gc thread during these tests.
Without a gc thread, any thread may run clause GC when it receives the
clause GC signal, e.g., the detached threads of test_cgc_1.pl, so we
retry.
*/

test_cgc_last :-
    run_tests([ cgc_last
              ]).

:- begin_tests(cgc_last,
                [ setup(stop_gc_thread(Old)),
                  cleanup(set_prolog_gc_thread(Old))
                ]).

:- dynamic fact/1, rule/1.

test(fact, S == S0) :-
    predicate_property(fact(_), size(S0)),
    assertz(fact(1)),
    retract(fact(1)),
    gc_to_size(fact(_), S0, 100, S).
test(rule, S == S0) :-
    predicate_property(rule(_), size(S0)),
    assertz((rule(X) :- succ_or_zero(X, a))),
    retract((rule(_) :- _)),
    gc_to_size(rule(_), S0, 100, S).

%   gc_to_size(+Head, +Expected, +Tries, -Size)
%
%   Run clause GC until the size of Head is Expected or we tried Tries
%   times.

gc_to_size(Head, Expected, Tries, Size) :-
    garbage_collect_clauses,
    predicate_property(Head, size(Size0)),
    (   ( Size0 == Expected ; Tries =< 1 )
    ->  Size = Size0
    ;   sleep(0.01),
        Tries1 is Tries - 1,
        gc_to_size(Head, Expected, Tries1, Size)
    ).

:- end_tests(cgc_last).

succ_or_zero(_, _).

stop_gc_thread(Old) :-
    current_prolog_flag(gc_thread, Old),
    set_prolog_gc_thread(false).
