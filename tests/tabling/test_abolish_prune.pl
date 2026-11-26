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

:- module(test_abolish_prune,
          [ test_abolish_prune/0
          ]).
:- use_module(library(plunit)).

/** <module> Test pruning the variant table after abolishing tables

Abolishing tables from trie_gen/3 on the variant table, as done by
abolish_table_subgoals/1 and untable/1, deletes the variants while the
variant trie is in use.  The deleted branches must be pruned when the
enumeration completes, so the call trie does not grow if new variants
are created and abolished repeatedly.
*/

test_abolish_prune :-
    run_tests([ abolish_prune
              ]).

:- table keep/1, pr/1.

keep(a).
pr(_).

vt_nodes(Nodes) :-
    (   '$tbl_local_variant_table'(VT)
    ->  trie_property(VT, node_count(Nodes))
    ;   Nodes = 0
    ).

fill(Round) :-
    forall(between(1, 1000, I),
           ( X is Round*1000000+I,
             pr(f(X, g(X)))
           )).

:- begin_tests(abolish_prune).

test(abolish_subgoals, true(Nodes1 =< Nodes0+10)) :-
    keep(_),
    vt_nodes(Nodes0),
    forall(between(1, 5, R),
           ( fill(R),
             abolish_table_subgoals(pr(_))
           )),
    vt_nodes(Nodes1).
test(untable, true(Nodes1 =< Nodes0+10)) :-
    keep(_),
    vt_nodes(Nodes0),
    forall(between(1, 5, R),
           ( fill(R),
             untable(test_abolish_prune:pr/1),
             table(test_abolish_prune:pr/1)
           )),
    vt_nodes(Nodes1).
test(reuse_after_prune, Answers == [x]) :-
    keep(_),
    fill(1),
    abolish_table_subgoals(pr(_)),
    fill(1),
    findall(x, pr(f(1000001, g(1000001))), Answers).

:- end_tests(abolish_prune).
