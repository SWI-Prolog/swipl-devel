/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
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


:- module(test_partial_compare, [test_partial_compare/0]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(random)).

/** <module> Test partial_compare/3

Test the variant of compare/3 that refuses to decide a comparison that
may be invalidated by binding a variable.
*/

test_partial_compare :-
    run_tests([ partial_compare
              ]).

:- begin_tests(partial_compare).

test(ground, D == (<)) :-
    partial_compare(D, a, b).
test(ground_compound, D == (<)) :-
    partial_compare(D, f(a,1), f(a,2)).
test(equal, D == (=)) :-
    partial_compare(D, f(a,X), f(a,X)).
test(same_var, D == (=)) :-
    partial_compare(D, X, X).
test(arity_decides, D == (<)) :-
    partial_compare(D, f(_,_), f(_,_,_)).
test(name_decides, D == (<)) :-
    partial_compare(D, f(_,1), g(_,2)).
test(shared_var_skipped, D == (<)) :-
    partial_compare(D, f(X,1), f(X,2)).

test(var_vs_atom, true) :-
    partial_compare(D, X, b),
    assertion(D == undecided(X,b)).
test(var_vs_var, true) :-
    partial_compare(D, X, Y),
    assertion(D == undecided(X,Y)).
test(first_undecided_pair, true) :-
    partial_compare(D, f(a,X,1), f(a,Y,2)),
    assertion(D == undecided(X,Y)).
test(nested, true) :-
    partial_compare(D, f(g(a,X)), f(g(a,b))),
    assertion(D == undecided(X,b)).
test(attvar, true) :-
    put_attr(V, test_partial_compare, 1),
    partial_compare(D, V, a),
    assertion(D == undecided(V,a)).
test(attvar_same, D == (=)) :-
    put_attr(V, test_partial_compare, 1),
    partial_compare(D, V, V).

                /*******************************
                *      ORDER IS SUPPLIED       *
                *******************************/

test(check_smaller) :-
    partial_compare(<, a, b).
test(check_larger, fail) :-
    partial_compare(>, a, b).
test(check_undecided) :-
    partial_compare(undecided(a,_), a, _).
test(check_undecided_fail, fail) :-
    partial_compare(undecided(b,_), a, _).
test(domain_atom, error(domain_error(partial_order, foo))) :-
    partial_compare(foo, a, b).
test(domain_compound, error(domain_error(partial_order, f(x)))) :-
    partial_compare(f(x), a, b).
test(domain_type, error(domain_error(partial_order, 1))) :-
    partial_compare(1, a, b).

                /*******************************
                *          INVARIANTS          *
                *******************************/

%  A decided answer must agree with compare/3 both now and after
%  arbitrary further instantiation.

test(stable) :-
    forall(between(1, 2000, _),
           ( random_term(T1),
             random_term(T2),
             partial_compare(D, T1, T2),
             (   D = undecided(_,_)
             ->  true
             ;   compare(D0, T1, T2),
                 assertion(D == D0),
                 copy_term(T1-T2, C1-C2),
                 term_variables(C1-C2, Vars),
                 bind_all(Vars),
                 compare(D1, C1, C2),
                 assertion(D == D1)
             ))).

%  An undecided pair must be a pair of subterms of the arguments and at
%  least one of them must be unbound.

test(culprit) :-
    forall(between(1, 2000, _),
           ( random_term(T1),
             random_term(T2),
             partial_compare(D, T1, T2),
             (   D = undecided(A,B)
             ->  assertion(( var(A) ; var(B) )),
                 assertion(contains_var(T1, A)),
                 assertion(contains_var(T2, B))
             ;   true
             ))).

%  Garbage collection while many undecided answers are alive.

test(gc, N == 20 000) :-
    numlist(1, 20 000, List),
    undecided_list(List, Ds),
    length(Ds, N).

undecided_list([], []).
undecided_list([_|T0], [D|T]) :-
    partial_compare(D, f(a,_), f(a,_)),
    D = undecided(_,_),
    undecided_list(T0, T).

bind_all([]).
bind_all([V|T]) :-
    random_term(V),
    bind_all(T).

contains_var(Term, Var) :-
    term_variables(Term, Vars),
    (   member(V, Vars),
        V == Var
    ->  true
    ;   \+ var(Var),
        contains_subterm(Term, Var)
    ).

contains_subterm(Term, Sub) :-
    Term == Sub,
    !.
contains_subterm(Term, Sub) :-
    compound(Term),
    arg(_, Term, Arg),
    contains_subterm(Arg, Sub),
    !.

random_term(Term) :-
    random_between(1, 7, Key),
    (   Key == 1
    ->  Term = _
    ;   Key == 2
    ->  random_between(1, 3, Term)
    ;   Key == 3
    ->  random_member(Term, [a,b,zz])
    ;   Key == 4
    ->  random_term(A),
        Term = f(A)
    ;   Key == 5
    ->  random_term(A),
        random_term(B),
        Term = g(A,B)
    ;   Key == 6
    ->  Term = "string"
    ;   random_term(A),
        Term = [A]
    ).

:- end_tests(partial_compare).
