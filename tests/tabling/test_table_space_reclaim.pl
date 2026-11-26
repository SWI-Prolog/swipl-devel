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

:- module(test_table_space_reclaim,
          [ test_table_space_reclaim/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).

/** <module> Test reclaimable table space

Abolished private answer tables that are still referenced by a Prolog
handle are owned by their symbol and released by atom-GC.  Their memory
is reported by statistics(table_space_reclaimable, Bytes) and does not
count for the table_space limit.
*/

test_table_space_reclaim :-
    run_tests([ table_space_reclaim
              ]).

%   Many small tables, such that the trie structs of the abolished
%   tables, which await atom-GC, are a significant part of the space.

:- table tsr/2.

tsr(N, f(N)).

fill(N) :-
    forall(between(1, N, I), forall(tsr(I, _), true)).

handles(Tries) :-
    findall(T, ( current_table(test_table_space_reclaim:G, T),
                 G = tsr(_,_)
               ), Tries).

used(U) :-
    statistics(table_space_used, U).
reclaimable(R) :-
    statistics(table_space_reclaimable, R).

%   Abolish the tables while we hold their handles.

abolish_held(R) :-
    handles(Tries),
    abolish_all_tables,
    reclaimable(R),
    length(Tries, _).

:- begin_tests(table_space_reclaim).

test(account, [ true(R1-R0 >= 1000*100), true(R2 < R1/10) ]) :-
    abolish_all_tables,
    garbage_collect_atoms,		% garbage of previous tests
    reclaimable(R0),
    fill(1000),
    findall(R, abolish_held(R), [R1]),
    garbage_collect_atoms,
    reclaimable(R2).
test(limit, [ true(var(E)) ]) :-
    abolish_all_tables,
    garbage_collect_atoms,
    current_prolog_flag(table_space, Old),
    used(U0),
    fill(5000),
    used(U1),
    Fill is U1-U0,
    handles(Tries),
    abolish_all_tables,
    used(Used),
    reclaimable(R),
    assertion(R > Fill/10),
    Limit is Used - R + Fill + Fill//10,
    setup_call_cleanup(
        set_prolog_flag(table_space, Limit),
        catch(fill(5000), E, true),
        set_prolog_flag(table_space, Old)),
    length(Tries, _),			% keep the handles alive
    abolish_all_tables.

:- end_tests(table_space_reclaim).
