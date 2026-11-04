/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(test_gc_generational,
          [ test_gc_generational/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Test generational (minor) stack garbage collection

Each test runs with the flag `gc_generational` set to `true`.  The
tests keep data alive across many automatic GCs and verify it is
intact.  Most tests cover a construct where old data is changed or
filled after a GC: bindings of old variables, setarg/3, nb_setarg/3,
global variables, exceptions, attributed variables and reading terms.

In debug builds, run with prolog_debug('MSG_GC_GEN_VERIFY') to verify
the old generation before each minor GC.
*/

test_gc_generational :-
    run_tests([ gc_generational
              ]).

:- meta_predicate
    gen(0).

gen(Goal) :-
    current_prolog_flag(gc_generational, Old),
    setup_call_cleanup(
        gen_on,
        once(Goal),
        gen_off(Old)).

gen_on :-
    set_prolog_flag(gc_generational, true),
    prolog_debug('MSG_GC_GEN_VERIFY').    % no-op in release builds

gen_off(Old) :-
    prolog_nodebug('MSG_GC_GEN_VERIFY'),
    set_prolog_flag(gc_generational, Old).

%!  scramble
%
%   Run several GCs and reuse the young part of the global stack. If
%   an old-to-young pointer was missed, it now points to other data.

scramble :-
    churn(300 000),
    big(50 000, L),
    churn(100 000),
    length(L, _).

minor_collections(Count) :-
    statistics(minor_collections, Count).

big(N, L) :-
    numlist(1, N, L0),
    maplist(wrap, L0, L).

wrap(X, f(X)).

check_big(L, N) :-
    length(L, N),
    foldl(check_f, L, 1, _).

check_f(f(X), X, X1) :-
    X1 is X+1.

churn(0) :- !.
churn(N) :-
    numlist(1, 20, L),
    sum_list(L, _),
    N1 is N-1,
    churn(N1).

:- begin_tests(gc_generational).

test(minor, [true(Minor > 0)]) :-
    gen(( minor_collections(M0),
          big(200 000, L),
          churn(100 000),
          check_big(L, 200 000),
          minor_collections(M1),
          Minor is M1-M0
        )).
test(bind_old_var, Last == done) :-
    gen(( length(L, 100 000),
          churn(100 000),               % L is now old
          maplist(=(x(y)), L),          % bind old vars to young terms
          churn(100 000),
          maplist(==(x(y)), L),
          dl(100 000, Tail, T),
          churn(100 000),
          T = [done],
          scramble,
          last(Tail, Last)
        )).
test(setarg, Sum == 5_000_050_000) :-
    gen(( length(Zeros, 100),
          maplist(=(0), Zeros),
          T =.. [t|Zeros],
          numlist(1, 100 000, L),
          foldl(add_arg(T), L, 0, _),
          scramble,
          T =.. [_|Args],
          sum_list(Args, Sum)
        )).
test(nb_setarg, List == Expected) :-
    gen(( big(100 000, Big),
          S = s(0, []),
          (   between(1, 100 000, I),
              numlist(1, 30, G),
              nb_setarg(1, S, I),
              nb_setarg(2, S, G),
              fail
          ;   true
          ),
          scramble,
          S = s(100 000, List),
          numlist(1, 30, Expected),
          check_big(Big, 100 000)
        )).
test(nb_setval, List == Expected) :-
    gen(( big(100 000, Big),
          (   between(1, 100 000, I),
              numlist(1, 30, G),
              nb_setval(gen_test, I-G),
              fail
          ;   true
          ),
          scramble,
          nb_getval(gen_test, 100 000-List),
          nb_delete(gen_test),
          numlist(1, 30, Expected),
          check_big(Big, 100 000)
        )).
test(b_setval, Len == 100 000) :-
    gen(( b_setval(gen_test, []),
          numlist(1, 100 000, L),
          foldl(push_bval, L, _, _),
          b_getval(gen_test, V),
          length(V, Len)
        )).
test(exception) :-
    gen(( big(100 000, Big),
          forall(between(1, 20 000, I),
                 ( numlist(1, 30, G),
                   catch(throw(e(I, G)), e(I, G2), true),
                   G2 == G
                 )),
          check_big(Big, 100 000)
        )).
test(freeze, Sum == 20_000_100_000) :-
    gen(( length(L, 200 000),
          maplist(freeze_add, L),
          churn(100 000),
          numlist(1, 200 000, L),
          nb_getval(gen_freeze_sum, Sum),
          nb_delete(gen_freeze_sum)
        )).
test(dif, true) :-
    gen(( length(L, 100 000),
          maplist(dif(a), L),
          churn(100 000),
          maplist(=(b), L)
        )).
test(attvar_update, Values == [3,3,3]) :-
    gen(( length(L, 3),
          maplist([X]>>put_attr(X, test_gc_generational, 1), L),
          churn(100 000),
          maplist([X]>>put_attr(X, test_gc_generational, 2), L),
          churn(100 000),
          maplist([X]>>put_attr(X, test_gc_generational, 3), L),
          maplist([X,V]>>get_attr(X, test_gc_generational, V), L, Values)
        )).
test(read_list, Len == 300 000) :-
    numlist(1, 300 000, L0),
    format(string(S), "~q.", [[a|L0]-[x|_]]),
    gen(( term_string(T, S),
          scramble,
          T = [a|L]-[x|_],
          length(L, Len),
          L == L0
        )).
test(backtrack, Count == 1000) :-
    gen(( big(100 000, Big),
          aggregate_all(count,
                        ( between(1, 1000, _),
                          numlist(1, 100, L),
                          member(X, L),
                          X == 100
                        ),
                        Count),
          check_big(Big, 100 000)
        )).

% The generational write barrier must not change what is undone on
% backtracking.  library(pure_input) depends on this: a binding of a
% cell newer than the last choicepoint is not trailed, so it survives
% backtracking if the cell survives (here through nb_linkarg/3).

test(untrailed_binding, X == bound) :-
    gen(untrailed_binding(X)).

:- end_tests(gc_generational).

% Must be a clause rather than a meta-called conjunction: in the
% latter, T is a variable of the goal term, which is older than the
% choicepoint and therefore its binding is trailed.

untrailed_binding(X) :-
    S = s(_),
    (   member(_, [1,2]),
        T = f(_),
        garbage_collect,                % f(_) becomes old
        T = f(bound),
        nb_linkarg(1, S, T),
        fail
    ;   true
    ),
    S = s(f(X)).

dl(0, T, T) :- !.
dl(N, [N|T0], T) :-
    N1 is N-1,
    dl(N1, T0, T).

add_arg(T, I, S0, S) :-
    A is I mod 100 + 1,
    arg(A, T, V0),
    V is V0+I,
    setarg(A, T, V),
    S is S0+I.

push_bval(X, _, _) :-
    b_getval(gen_test, V0),
    b_setval(gen_test, [X|V0]).

freeze_add(X) :-
    freeze(X, add_freeze(X)).

add_freeze(X) :-
    (   nb_current(gen_freeze_sum, S0)
    ->  true
    ;   S0 = 0
    ),
    S is S0+X,
    nb_setval(gen_freeze_sum, S).

attr_unify_hook(_, _).
