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

:- module(test_wrap_predicate, [test_wrap_predicate/0]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(prolog_wrap)).
:- use_module(library(prolog_trace)).

/** <module> Test wrap_predicate/4 on meta and SSU predicates

Regression tests for the interaction between wrap_predicate/4 and
predicates that require a supervisor prefix (meta-predicates and
SSU/det predicates).  See issue #1245.
*/

test_wrap_predicate :-
    run_tests([ wrap_meta,
                wrap_trace_meta,
                wrap_ssu,
                wrap_det
              ]).

my_plus(A, B, C) :- C is A + B.


:- begin_tests(wrap_meta).

% Wrap a meta-predicate directly and verify the meta-argument is
% qualified in the caller's module, so the wrapped goal succeeds.

:- meta_predicate mp_call(1, +, -).
mp_call(G, X, Y) :- call(G, X, Y).

test(direct, Y == 42) :-
    wrap_predicate(mp_call(_,_,_), mark, Wrapped, Wrapped),
    mp_call(succ, 41, Y),
    unwrap_predicate(mp_call(_,_,_), mark).
test(chain, Ys == [11,12,13]) :-
    wrap_predicate(mp_call(_,_,_), a, W1, W1),
    wrap_predicate(mp_call(_,_,_), b, W2, W2),
    findall(Y, ( member(X, [10,11,12]), mp_call(succ, X, Y) ), Ys),
    unwrap_predicate(mp_call(_,_,_), b),
    unwrap_predicate(mp_call(_,_,_), a).
test(wrapped_prop, Names == [a]) :-
    wrap_predicate(mp_call(_,_,_), a, W, W),
    predicate_property(mp_call(_,_,_), wrapped(Names)),
    unwrap_predicate(mp_call(_,_,_), a).

:- end_tests(wrap_meta).


:- begin_tests(wrap_trace_meta).

% Issue #1245: tracing foldl/4 (a meta-predicate) must qualify its
% closure argument in the caller module so my_plus/3 (in user) is found.

silent_hook(frame(_, trace(_, _)), _, _).

test(trace_foldl, N == 6) :-
    asserta((user:thread_message_hook(M, K, L) :- silent_hook(M, K, L)), Ref),
    call_cleanup(
        setup_call_cleanup(
            trace(foldl/4),
            foldl(my_plus, [1,2,3], 0, N),
            unwrap_predicate(foldl(_,_,_,_), trace)),
        erase(Ref)).

test(retrace_survives_unwrap, N == 60) :-
    asserta((user:thread_message_hook(M, K, L) :- silent_hook(M, K, L)), Ref),
    call_cleanup(
        (   trace(foldl/4),
            foldl(my_plus, [1,2,3], 0, _),
            unwrap_predicate(foldl(_,_,_,_), trace),
            foldl(my_plus, [10,20,30], 0, N)
        ),
        erase(Ref)).

:- end_tests(wrap_trace_meta).


:- begin_tests(wrap_ssu).

% Wrap an SSU predicate.  The S_SSU_DET prefix that guards single-sided
% unification must still take effect through the wrapper.

ssu(a) => x = a.
ssu(b) => x = b.

test(preserve_ssu, X == a) :-
    setup_call_cleanup(
        wrap_predicate(ssu(_), mark, Wrapped, Wrapped),
        ssu(a) = ssu(X),
        unwrap_predicate(ssu(_), mark)).
test(ssu_second, X == b) :-
    setup_call_cleanup(
        wrap_predicate(ssu(_), mark, Wrapped, Wrapped),
        ssu(b) = ssu(X),
        unwrap_predicate(ssu(_), mark)).
test(ssu_no_match, error(existence_error(matching_rule, _))) :-
    setup_call_cleanup(
        wrap_predicate(ssu(_), mark, Wrapped, Wrapped),
        ssu(c),
        unwrap_predicate(ssu(_), mark)).

:- end_tests(wrap_ssu).


:- begin_tests(wrap_det).

% Wrap a :- det/1 predicate.  The S_DET prefix must still cause a
% determinism_error when the wrapped predicate returns non-det.

:- det(dp/1).
dp(1).
dp(2).

test(det_ok) :-
    setup_call_cleanup(
        wrap_predicate(dp(_), mark, Wrapped, Wrapped),
        dp(1),
        unwrap_predicate(dp(_), mark)).
test(det_violation,
     error(determinism_error(_:dp/1, det, nondet, property))) :-
    setup_call_cleanup(
        wrap_predicate(dp(_), mark, Wrapped, Wrapped),
        dp(_),
        unwrap_predicate(dp(_), mark)).

:- end_tests(wrap_det).
