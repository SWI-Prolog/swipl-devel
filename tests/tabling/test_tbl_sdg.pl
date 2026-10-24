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


:- module(test_tbl_sdg,
          [ test_tbl_sdg/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(thread)).

/** <module> Test '$tbl_sdg'/3, the subgoal dependency graph

The tabled predicates below call take/0 while their tables are
incomplete.  take/0 stores '$tbl_sdg'/3 of that moment, which sdg/4
translates from tries and components to the variants and their
depth in the component tree.
*/

test_tbl_sdg :-
    run_tests([ tbl_sdg
              ]).

:- dynamic snapshot/1.

take :-
    '$tbl_sdg'(Components, Tables, Edges),
    assertz(snapshot(sdg(Components, Tables, Edges))).

%   sdg(:Goal, -Components, -Tables, -Edges)
%
%   Run Goal and translate the first snapshot.  Components is a list
%   Depth-Leader, Tables a list table(Variant, Depth, Answers, Flags)
%   and Edges a list edge(Caller, Callee, Sign, Count), all sorted and
%   with the variants numbered.

sdg(Goal, Components, Tables, Edges) :-
    retractall(snapshot(_)),
    abolish_all_tables,
    ignore(Goal),
    snapshot(sdg(Cs, Ts, Es)),
    !,
    retractall(snapshot(_)),
    maplist(table_variant, Ts, Map),
    maplist(component(Cs, Map), Cs, Components0),
    maplist(table(Cs), Ts, Tables0),
    maplist(edge(Map), Es, Edges0),
    msort(Components0, Components),
    msort(Tables0, Tables),
    msort(Edges0, Edges).

table_variant(table(Trie, Variant, _, _, _), Trie-V) :-
    strip(Variant, V).

strip(_:G0, G) :-
    !,
    copy_term(G0, G),
    numbervars(G, 0, _).
strip(G, G).

component(Cs, Map, scc(SCC, _, Leader), Depth-L) :-
    depth(Cs, SCC, Depth),
    memberchk(Leader-L, Map).

depth(Cs, SCC, Depth) :-
    memberchk(scc(SCC, Parent, _), Cs),
    (   Parent == null
    ->  Depth = 0
    ;   depth(Cs, Parent, Depth0),
        Depth is Depth0+1
    ).

table(Cs, table(_, Variant, SCC, Answers, Flags0),
      table(V, Depth, Answers, Flags)) :-
    strip(Variant, V),
    depth(Cs, SCC, Depth),
    msort(Flags0, Flags).

edge(Map, edge(From, To, Sign, Count), edge(F, T, Sign, Count)) :-
    memberchk(From-F, Map),
    memberchk(To-T, Map).

:- table p/1, q/1.                      % a positive loop
p(X) :- q(X).
p(1).
q(X) :- p(X).
q(2) :- take.

:- table r/0, s/0.                      % a negative loop
r :- tnot(s).
s :- tnot(r), take.
s :- tnot(r).

:- table c1/1, c2/1.                    % c2(_) is a child component
c1(X) :- c2(X), c1(X).
c1(2).
c2(X) :- c2(X).
c2(1) :- take.

:- table a/1, b/1.                      % break/0 hides the component;
a(X) :- b(X).                           % in_break/1 makes it read take/0
b(X) :- a(X).
b(1) :- break.

in_break(Goal) :-
    open_string("test_tbl_sdg:take.\n", In),
    set_stream(In, alias(user_input)),
    open_null_stream(Null),
    set_stream(Null, alias(user_output)),
    set_stream(Null, alias(user_error)),
    call(Goal).

:- table w/1.                           % w/1 waits for a message
w(X) :- w(X).
w(1) :- thread_get_message(go).

:- begin_tests(tbl_sdg).

test(none, Cs-Ts-Es == []-[]-[]) :-
    '$tbl_sdg'(Cs, Ts, Es).
test(positive, [ Cs == [0-p('$VAR'(0))],
                 Ts == [ table(p('$VAR'(0)), 0, 0, [pending]),
                         table(q('$VAR'(0)), 0, 0, [pending])
                       ],
                 Es == [edge(q('$VAR'(0)), p('$VAR'(0)), positive, 1)]
               ]) :-
    sdg(p(_), Cs, Ts, Es).
test(negative, [ Ts == [ table(r, 0, 1, [delays, negative]),
                         table(s, 0, 1, [delays, negative, pending])
                       ],
                 Negative == [edge(r, s, negative, 1),
                              edge(s, r, negative, 2)]
               ]) :-
    sdg(r, _, Ts, Es),
    include([edge(_,_,Sign,_)]>>(Sign == negative), Es, Negative).
test(child, [ Cs == [0-c1('$VAR'(0)), 1-c2('$VAR'(0))],
              Ts == [ table(c1('$VAR'(0)), 0, 0, [pending]),
                      table(c2('$VAR'(0)), 1, 0, [pending])
                    ]
            ]) :-
    sdg(c1(_), Cs, Ts, _).
test(hidden, Ts == [ table(a('$VAR'(0)), 0, 0, [pending]),
                     table(b('$VAR'(0)), 0, 0, [pending])
                   ]) :-
    sdg(( thread_create(in_break(a(_)), Id),
          thread_join(Id)
        ), _, Ts, _).
test(other_thread, [ true(length(Cs, 1)),
                     V-Es == w('$VAR'(0))-[edge(T, T, positive, 1)]
                   ]) :-
    abolish_all_tables,
    thread_create(w(_), Id),
    wait_for_table(Id, 50, Cs, [table(T, V0, _, _, _)], Es),
    thread_send_message(Id, go),
    thread_join(Id),
    strip(V0, V).

%   Wait until thread Id runs w/1: at most Tries times 0.1 sec.

wait_for_table(Id, Tries, Cs, Ts, Es) :-
    call_in_thread(Id, '$tbl_sdg'(Cs0, Ts0, Es0)),
    (   Ts0 \== []
    ->  Cs = Cs0, Ts = Ts0, Es = Es0
    ;   Tries > 0
    ->  sleep(0.1),
        Tries1 is Tries-1,
        wait_for_table(Id, Tries1, Cs, Ts, Es)
    ).

:- end_tests(tbl_sdg).
