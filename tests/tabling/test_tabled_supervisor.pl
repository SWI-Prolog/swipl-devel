/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
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

:- module(test_tabled_supervisor,
          [ test_tabled_supervisor/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(prolog_wrap)).

/** <module> Test the S_TABLED supervisor

Calls to a variant tabled predicate whose table is complete are answered
by the S_TABLED supervisor from the compiled answer trie, bypassing
start_tabling/3.  These tests verify that the answers, dependencies and
side conditions are the same as with the wrapper.
*/

test_tabled_supervisor :-
    run_tests([ tabled_supervisor
              ]).

:- meta_predicate
    all(?, 0, -),
    inferences(0, -).

all(T, G, L) :- findall(T, G, L0), msort(L0, L).

%   inferences(:Goal, -Count) is semidet.
%
%   Count is the number of inferences for once(Goal), compensated
%   for the overhead.  A call that is answered by S_TABLED costs
%   one inference, the same as a call to a fact.

inferences(G, N) :-
    statistics(inferences, I0),
    once(G),
    statistics(inferences, I1),
    statistics(inferences, I2),
    N is (I1-I0)-(I2-I1).

:- begin_tests(tabled_supervisor).

:- dynamic
    counter/1,
    d/1 as incremental,
    md/1 as monotonic.

fact(a).

:- table
    f/2, g/1, e/1, z/0, nz/0, pa/2, big/3, lv/1, gc/1,
    w/0, nw/0, wrapped/1, cnt/1.

f(N, X) :- between(1, N, X).
g(a).
e(_) :- fail.
z.
nz :- fail.
pa(a, 1).
pa(a, 2).
pa(b, f(_)).
big(X, s(Y), L) :- between(1, 3, X), Y = X, numlist(1, 1000, L).
gc(L) :- between(1, 100, X), length(L0, X), L = [X|L0].
w :- tnot(nw).
nw :- tnot(w).
wrapped(X) :- between(1, 3, X).
cnt(X) :- flag(cnt, N, N+1), X = N.

lv(Y) :- f(3, X), Y = X.               % X is a local variable

:- table (ip/1, iq/1) as incremental.
ip(X) :- d(X).
iq(X) :- ip(X).

:- table (mp/1, mq/1) as monotonic.
mp(X) :- md(X).
mq(X) :- mp(X).

:- table sh/1 as shared.
sh(X) :- between(1, 3, X).

:- table rv/2, ind/2, mix/2.
rv(X, Y) :- member(X-Y, [1-1, 1-2, _-_]).
ind(X, Y) :- member(X-Y, [1.5-float, "str"-string, 100000000000000000000-big,
                          a-atom, 1-int]).
mix(X, Y) :- member(X-Y, [f(1)-c1, f(_)-c2, [a]-list, a-atom]).

:- table only/1, only0/0, other/1.
other(other).
only(X) :- flag(only, N, N+1), X = N.
only0 :- flag(only, N, N+1), N >= 0.

:- table mo(_,max).
mo(a, 1).
mo(a, 3).
mo(a, 2).

:- table su/1 as subsumptive.
su(X) :- between(1, 3, X).

test(supervisor) :-
    assertion('$get_predicate_attribute'(f(_,_), tabled_variant, 1)),
    assertion('$get_predicate_attribute'(sh(_), tabled_variant, 1)),
    assertion(\+ '$get_predicate_attribute'(mo(_,_), tabled_variant, 1)),
    assertion(\+ '$get_predicate_attribute'(su(_), tabled_variant, 1)).
test(fast_path) :-
    g(a),
    inferences(fact(a), N0),
    inferences(g(a), N1),
    assertion(N0 == N1).
test(fast_path_shared, condition(current_prolog_flag(threads, true))) :-
    all(X, sh(X), _),
    inferences(sh(_), N0),
    inferences(fact(_), N1),
    assertion(N0 == N1).
test(fast_path_nonground) :-
    all(X, f(3, X), _),
    inferences(f(3, _), N0),
    inferences(fact(_), N1),
    assertion(N0 == N1).
test(fast_path_indirect) :-
    forall(member(K, [1.5, "str", 100000000000000000000]),
           ( all(T, ind(K, T), _),
             inferences(ind(K, _), N0),
             inferences(fact(_), N1),
             assertion(N0 == N1)
           )).
test(repeated_vars) :-
    all(X-Y, rv(X, Y), L0),             % create both variant tables
    all(X, rv(X, X), L1),
    all(X-Y, rv(X, Y), L2),
    all(X, rv(X, X), L3),
    assertion(L0 =@= L2),
    assertion(L1 =@= L3),
    assertion(length(L1, 2)),
    inferences(rv(_, _), N0),
    inferences(rv(Z, Z), N1),
    inferences(fact(_), N2),
    assertion(N0 == N2),
    assertion(N1 == N2),
    assertion(var(Z)).
test(indirect) :-
    forall(member(K-T, [1.5-float, "str"-string, 100000000000000000000-big,
                        a-atom, 1-int]),
           ( all(T1, ind(K, T1), L0),
             all(T1, ind(K, T1), L1),
             assertion(L0 == [T]),
             assertion(L1 == L0)
           )),
    assertion(\+ ind(2.5, _)),
    assertion(\+ ind(2.5, _)),
    assertion(\+ ind("nostr", _)),
    assertion(\+ ind("nostr", _)).
test(compound_arg) :-
    all(T, mix(f(1), T), L0), all(T, mix(f(1), T), L1),
    assertion(L0 == [c1,c2]), assertion(L1 == L0),
    all(T, mix(f(_), T), L2), all(T, mix(f(_), T), L3),
    assertion(L2 == [c1,c2]), assertion(L3 == L2),
    all(X-T, mix(X, T), L4), all(X-T, mix(X, T), L5),
    assertion(length(L4, 4)), assertion(L5 =@= L4).
test(attvar_arg) :-                    % generic path; same as wrapper
    freeze(X, true),
    findall(X, f(3, X), L0),
    findall(X, f(3, X), L1),
    setup_call_cleanup(debug, findall(X, f(3, X), L2), nodebug),
    assertion(L0 == L1),
    assertion(L1 == L2).
test(abolish_pinned) :-                % abolish the only table of only/1
    flag(only, _, 0),
    only(X0),
    abolish_table_subgoals(only(_)),
    only(X1),                           % recomputed below the pinned node
    only(X2),                           % fast path
    assertion(X1 \== X0),
    assertion(X2 == X1),
    inferences(only(_), N0),
    inferences(fact(_), N1),
    assertion(N0 == N1),
    only0,
    abolish_table_subgoals(only0),
    only0, only0,
    inferences(only0, N2),
    assertion(N2 == N1).
test(abolish_reuse) :-                 % freed nodes are reused by other/1
    abolish_all_tables,
    flag(only, _, 0),
    only(_), only(_),                   % cache the start node
    abolish_table_subgoals(only(_)),
    abolish_table_subgoals(other(_)),
    other(O),
    only(X1),
    only(X2),
    assertion(O == other),
    assertion(integer(X1)),
    assertion(X2 == X1),
    inferences(only(_), N0),            % still uses the start node
    inferences(fact(_), N1),
    assertion(N0 == N1).
test(abolish_all_cache) :-
    abolish_all_tables,
    flag(only, _, 0),
    only(X0),
    abolish_all_tables,                 % frees the private variant table
    only(X1),
    abolish_all_tables,
    all(Y, f(4, Y), L),
    only(X2),
    assertion(X0 \== X1),
    assertion(X1 \== X2),
    assertion(L == [1,2,3,4]),
    inferences(only(_), N0),
    inferences(fact(_), N1),
    assertion(N0 == N1).
test(thread_cache, condition(current_prolog_flag(threads, true))) :-
    all(Y, f(4, Y), L0),
    forall(between(1, 5, _),
           ( thread_create(( all(Y, f(4, Y), L1),
                             all(Y, f(4, Y), L2),
                             assertion(L1 == L0),
                             assertion(L2 == L0)
                           ), Id),
             thread_join(Id, Status),
             assertion(Status == true)
           )),
    all(Y, f(4, Y), L3),
    assertion(L3 == L0).
test(shared_abolish, condition(current_prolog_flag(threads, true))) :-
    all(X, sh(X), L0),
    abolish_table_subgoals(sh(_)),
    all(X, sh(X), L1),
    all(X, sh(X), L2),
    assertion(L0 == [1,2,3]),
    assertion(L1 == L0),
    assertion(L2 == L0).
test(slow_path_debug) :-
    g(a),
    setup_call_cleanup(debug, inferences(g(a), N), nodebug),
    assertion(N > 1).
test(answers) :-
    all(X, f(5, X), L0), all(X, f(5, X), L1),
    assertion(L0 == [1,2,3,4,5]),
    assertion(L1 == L0),
    once(f(5, Y)),
    setup_call_cleanup(debug, once(f(5, Y1)), nodebug),
    assertion(Y == Y1),                 % same order as the wrapper
    assertion(f(5, 3)),
    assertion(\+ f(5, 6)).
test(partial) :-
    all(X-Y, pa(X, Y), _),
    assertion(all(Y, pa(a, Y), [1,2])),
    assertion(all(Y, pa(a, Y), [1,2])),
    assertion(\+ pa(c, _)),
    pa(b, T), assertion(T = f(V)), assertion(var(V)).
test(ground) :-
    g(a), g(a),
    assertion(\+ g(b)), assertion(\+ g(b)).
test(empty) :-
    assertion(\+ e(_)), assertion(\+ e(_)),
    assertion(\+ nz), assertion(\+ nz).
test(arity0) :-
    z, z.
test(local_var) :-
    all(Y, lv(Y), L0),
    all(Y, lv(Y), L1),
    assertion(L0 == [1,2,3]),
    assertion(L1 == L0).
test(big) :-
    all(X-Y, big(X, s(Y), _), _),
    all(X-Y-N, (big(X, s(Y), L), length(L, N)), R),
    assertion(R == [1-1-1000, 2-2-1000, 3-3-1000]),
    big(2, Z, _), assertion(Z == s(2)).
test(gc) :-
    all(L, gc(L), _),
    forall(gc(L), (garbage_collect, L = [X|T], length(T, X))).
test(no_reeval) :-
    flag(cnt, _, 0),
    cnt(X), cnt(Y),
    assertion(X == Y),
    abolish_all_tables,
    cnt(Z),
    assertion(Z \== X).
test(wfs) :-
    call_delays(w, D0),
    assertion(D0 \== true),
    call_delays(w, D1),
    assertion(D1 =@= D0).
test(outer_wrapper,
     [ setup(wrap_predicate(wrapped(X), outer, W,
                            (flag(outer, N, N+1), W))),
       cleanup(unwrap_predicate(wrapped/1, outer))
     ]) :-
    flag(outer, _, 0),
    all(X, wrapped(X), L0),
    all(X, wrapped(X), L1),
    assertion(L0 == [1,2,3]),
    assertion(L1 == L0),
    flag(outer, C, C),
    assertion(C == 2).
test(incremental, [cleanup((retractall(d(_)), abolish_all_tables))]) :-
    retractall(d(_)), abolish_all_tables,
    assertz(d(1)),
    all(X, ip(X), [1]),                 % complete ip/1 first
    assertion(all(X, iq(X), [1])),      % iq/1 calls complete ip/1
    assertz(d(2)),
    assertion(all(X, iq(X), [1,2])),
    retract(d(1)),
    assertion(all(X, iq(X), [2])).
test(monotonic, [cleanup((retractall(md(_)), abolish_all_tables))]) :-
    retractall(md(_)), abolish_all_tables,
    assertz(md(1)),
    all(X, mp(X), [1]),
    assertion(all(X, mq(X), [1])),
    assertz(md(2)),
    assertion(all(X, mq(X), [1,2])),
    assertion(all(X, mp(X), [1,2])).
test(shared_thread, condition(current_prolog_flag(threads, true))) :-
    all(X, sh(X), L0),
    thread_create(all(X, sh(X), L), Id),
    thread_join(Id, Status),
    assertion(Status == true),
    assertion(L0 == [1,2,3]),
    var(L).
test(moded) :-
    all(X, mo(a, X), [3]),
    all(X, mo(a, X), [3]).
test(subsumptive) :-
    all(X, su(X), [1,2,3]),
    assertion(su(2)),
    assertion(\+ su(4)).
test(retable,
     [ cleanup(abolish_all_tables)
     ]) :-
    M = test_tabled_supervisor,
    retractall(M:rt(_,_)),
    assertz(M:rt(a, 1)), assertz(M:rt(a, 3)), assertz(M:rt(a, 2)),
    compile_predicates([M:rt/2]),
    table(M:rt/2),
    assertion('$get_predicate_attribute'(M:rt(_,_), tabled_variant, 1)),
    assertion(all(Y, M:rt(a, Y), [1,2,3])),
    untable(M:rt/2),
    table(M:rt(_,max)),
    assertion(\+ '$get_predicate_attribute'(M:rt(_,_), tabled_variant, 1)),
    assertion(all(Y, M:rt(a, Y), [3])),
    assertion(all(Y, M:rt(a, Y), [3])).

:- end_tests(tabled_supervisor).
