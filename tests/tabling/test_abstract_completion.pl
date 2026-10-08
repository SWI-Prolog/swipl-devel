/*  Part of SWI-Prolog

    Author:        MesTTo
    Copyright (c)  2026, MesTTo
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

:- module(test_abstract_completion,
          [ test_abstract_completion/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(random)).

/** <module> Completion of recursive abstract and subsumptive tables

A recursive call to a subgoal-abstracted or subsumptive table that is
still incomplete suspends on that table, and completion offers each new
answer to the suspended calls.  The calls to a subgoal_abstract(0) table
bind arguments inside an abstracted compound, as in path(args(Y,_)), so
completion indexes the suspensions on keys along paths into the
arguments, a few paths per table chosen by how well they split the
suspended calls.  These tests compare such tables with the same
relations tabled as variants, on random graphs: recursion that binds
the first or the second argument inside the compound, both in one
table, nodes wrapped in a compound so the keys are a level deeper,
calls that bind none of them, answers that leave a bound argument
open, nodes of several shapes, some nested deeper than the keys reach,
nodes that are random ground terms, a subsumptive table whose calls
bind an argument, and conditional answers under the well-founded
semantics.

subgoal_abstract(N) abstracts a call only while the thread's flag
max_table_subgoal_size_action is `abstract`, so the unit sets the flag
and restores it afterwards.
*/

test_abstract_completion :-
    run_tests([ abstract_completion
              ]).

:- begin_tests(abstract_completion,
               [ setup(abstract_subgoals),
                 cleanup(restore_subgoal_action)
               ]).

:- dynamic edge/2, hub/1, loop/1, link/2, node_term/2, term_link/2.

:- table path/1 as subgoal_abstract(0).
:- table rpath/1 as subgoal_abstract(0).
:- table mpath/1 as subgoal_abstract(0).
:- table dpath/1 as subgoal_abstract(0).
:- table hpath/1 as subgoal_abstract(0).
:- table wpath/1 as subgoal_abstract(0).
:- table xpath/1 as subgoal_abstract(0).
:- table tpath/1 as subgoal_abstract(0).
:- table upath/1 as subgoal_abstract(0).
:- table spath/2 as subsumptive.
:- table vpath/2, vrpath/2, vhpath/2, vwpath/2, vxpath/2, vtpath/2, vupath/2.

path(args(X, Y)) :- edge(X, Y).
path(args(X, Z)) :- edge(X, Y), path(args(Y, Z)).      % binds the first

rpath(args(X, Y)) :- edge(X, Y).
rpath(args(X, Z)) :- edge(Y, Z), rpath(args(X, Y)).    % binds the second

mpath(args(X, Y)) :- edge(X, Y).
mpath(args(X, Z)) :- edge(X, Y), mpath(args(Y, Z)).    % binds the first
mpath(args(X, Z)) :- edge(Y, Z), mpath(args(X, Y)).    % and the second

dpath(args(n(X), n(Y))) :- edge(X, Y).
dpath(args(n(X), n(Z))) :- edge(X, Y), dpath(args(n(Y), n(Z))).

hpath(args(X, Y)) :- edge(X, Y).
hpath(args(X, Z)) :- edge(X, Y), hpath(args(Y, Z)).
hpath(args(X, Z)) :- hub(X), hpath(args(_, Z)).        % binds neither

wpath(args(X, Y)) :- edge(X, Y).
wpath(args(X, _)) :- hub(X).                            % open at the second
wpath(args(X, Z)) :- edge(Y, Z), wpath(args(X, Y)).

xpath(args(X, Y)) :- link(X, Y).
xpath(args(X, Z)) :- link(X, Y), xpath(args(Y, Z)).

tpath(args(X, Y)) :- term_link(X, Y).
tpath(args(X, Z)) :- term_link(X, Y), tpath(args(Y, Z)).

upath(args(X, Y)) :- edge(X, Y).
upath(args(X, Z)) :- edge(X, Y), upath(args(Y, Z)).
upath(args(X, X)) :- loop(X), undefined.

spath(X, Y) :- edge(X, Y).
spath(X, Z) :- edge(X, Y), spath(Y, Z).                 % a subsumed call

vpath(X, Y) :- edge(X, Y).
vpath(X, Z) :- edge(X, Y), vpath(Y, Z).

vrpath(X, Y) :- edge(X, Y).
vrpath(X, Z) :- edge(Y, Z), vrpath(X, Y).

vhpath(X, Y) :- edge(X, Y).
vhpath(X, Z) :- edge(X, Y), vhpath(Y, Z).
vhpath(X, Z) :- hub(X), vhpath(_, Z).

vwpath(X, Y) :- edge(X, Y).
vwpath(X, _) :- hub(X).
vwpath(X, Z) :- edge(Y, Z), vwpath(X, Y).

vxpath(X, Y) :- link(X, Y).
vxpath(X, Z) :- link(X, Y), vxpath(Y, Z).

vtpath(X, Y) :- term_link(X, Y).
vtpath(X, Z) :- term_link(X, Y), vtpath(Y, Z).

vupath(X, Y) :- edge(X, Y).
vupath(X, Z) :- edge(X, Y), vupath(Y, Z).
vupath(X, X) :- loop(X), undefined.

abstract_subgoals :-
    current_prolog_flag(max_table_subgoal_size_action, Action),
    nb_setval(abstract_completion_action, Action),
    set_prolog_flag(max_table_subgoal_size_action, abstract).

restore_subgoal_action :-
    nb_getval(abstract_completion_action, Action),
    set_prolog_flag(max_table_subgoal_size_action, Action).

%!  graph(+Seed)
%
%   A random graph of 60 edges over 30 nodes, with two hubs and two
%   nodes with an undefined loop, fresh tables and the same edges for
%   the same Seed.  link/2 holds the same edges between the nodes'
%   shapes, and term_link/2 between random terms standing for them.

graph(Seed) :-
    abolish_all_tables,
    retractall(edge(_,_)),
    retractall(hub(_)),
    retractall(loop(_)),
    retractall(link(_,_)),
    retractall(node_term(_,_)),
    retractall(term_link(_,_)),
    set_random(seed(Seed)),
    forall(between(1, 60, _),
           ( random_between(1, 30, X),
             random_between(1, 30, Y),
             assertz(edge(X, Y)),
             shape(X, SX),
             shape(Y, SY),
             assertz(link(SX, SY)) )),
    forall(between(1, 2, _),
           ( random_between(1, 30, H), assertz(hub(H)),
             random_between(1, 30, L), assertz(loop(L)) )),
    forall(between(1, 30, N),
           ( random_term(N, T), assertz(node_term(N, T)) )),
    forall(edge(X, Y),
           ( node_term(X, TX), node_term(Y, TY), assertz(term_link(TX, TY)) )).

%!  shape(+N, -Node)
%
%   Node N as a number, wrapped in n/1, or nested in k/1 below the four
%   levels that suspension keys reach, so a table of links mixes paths
%   that an answer of another shape does not have.

shape(N, k(k(k(k(N))))) :- N mod 7 =:= 0, !.
shape(N, N) :- N mod 5 =:= 0, !.
shape(N, n(N)).

%!  random_term(+N, -Term)
%
%   Node N inside a random ground term of up to six levels of f/1, g/2
%   and h/3, N at one leaf and the other leaves atoms, strings, floats
%   or big integers, so the paths that tell nodes apart run past the
%   four levels keys reach and through compounds of other arities.

random_term(N, Term) :-
    random_between(0, 6, Depth),
    random_term(Depth, N, Term).

random_term(0, N, N) :- !.
random_term(Depth, N, Term) :-
    random_member(F/A, [f/1, g/2, h/3]),
    random_between(1, A, I),
    Depth1 is Depth - 1,
    random_term(Depth1, N, Sub),
    length(Args, A),
    nth1(I, Args, Sub),
    maplist(random_leaf, Args),
    Term =.. [F|Args].

random_leaf(Leaf) :-
    nonvar(Leaf),
    !.
random_leaf(Leaf) :-
    random_member(Leaf, [a, b, "s", "t", 1.5, 2.5,
                         18446744073709551617, 36893488147419103233]).

seed(Seed) :-
    between(1, 8, Seed).

%!  shape_seed(-Seed)
%
%   The seeds of the random-term graphs, more than seed/1 gives since
%   each draws new term shapes and costs a few milliseconds.

shape_seed(Seed) :-
    between(1, 64, Seed).

pairs(Template, Goal, Pairs) :-
    findall(Template, Goal, List),
    sort(List, Pairs).

%!  node_pairs(+Goal, ?X, ?Y, -Pairs)
%
%   The pairs of nodes Goal relates, an open argument standing for
%   every node.

node_pairs(Goal, X, Y, Pairs) :-
    pairs(X-Y, ( Goal, between(1, 30, X), between(1, 30, Y) ), Pairs).

truth(true, true) :- !.
truth(_, undefined).

test(first_bound, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, path(args(X, Y)), Abstract),
    pairs(X-Y, vpath(X, Y), Variant).
test(second_bound, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, rpath(args(X, Y)), Abstract),
    pairs(X-Y, vrpath(X, Y), Variant).
test(both_bound, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, mpath(args(X, Y)), Abstract),
    pairs(X-Y, vpath(X, Y), Variant).
test(deep_bound, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, dpath(args(n(X), n(Y))), Abstract),
    pairs(X-Y, vpath(X, Y), Variant).
test(none_bound, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, hpath(args(X, Y)), Abstract),
    pairs(X-Y, vhpath(X, Y), Variant).
test(open_answers, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    node_pairs(wpath(args(X, Y)), X, Y, Abstract),
    node_pairs(vwpath(X, Y), X, Y, Variant).
test(node_shapes, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, xpath(args(X, Y)), Abstract),
    pairs(X-Y, vxpath(X, Y), Variant).
test(random_shapes, [forall(shape_seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, tpath(args(X, Y)), Abstract),
    pairs(X-Y, vtpath(X, Y), Variant).
test(bound_read, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y, ( between(1, 30, X), path(args(X, Y)) ), Abstract),
    pairs(X-Y, vpath(X, Y), Variant).
test(subsumptive, [forall(seed(Seed)), true(Subsumptive == Variant)]) :-
    graph(Seed),
    pairs(X-Y, spath(X, Y), Subsumptive),
    pairs(X-Y, vpath(X, Y), Variant).
test(undefined, [forall(seed(Seed)), true(Abstract == Variant)]) :-
    graph(Seed),
    pairs(X-Y-T, ( call_delays(upath(args(X, Y)), D), truth(D, T) ), Abstract),
    pairs(X-Y-T, ( call_delays(vupath(X, Y), D), truth(D, T) ), Variant).

:- end_tests(abstract_completion).
