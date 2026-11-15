/*  Summarise the private tables of the calling thread.  See
    ../ergoai-cyc150.md.
*/

:- module(tblstat, [tblstat/0, tblstat/1]).
:- use_module(library(aggregate)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(lists)).

%   tblstat(+Top)
%
%   Summarise the private tables of the calling thread: totals, then the
%   Top predicates by bytes.

tblstat :- tblstat(25).
tblstat(Top) :-
    retractall(st(_,_)),
    statistics(table_space_used, Pool),
    thread_self(Me),
    '$idg_thread_space'(Me, IDG),
    '$idg_thread_summary'(Me, IDGNodes, IDGEdges),
    empty_nb_set_count(Acc),
    (   '$tbl_local_variant_table'(VT)
    ->  '$trie_property'(VT, size(VTSize)),
        '$trie_property'(VT, value_count(NTables)),
        forall(trie_gen(VT, M:V, T), add(Acc, M, V, T))
    ;   VTSize = 0,
        NTables = 0
    ),
    Acc = acc(Preds0, Tot),
    format("pool ~D  call-trie ~D  IDG ~D (~D nodes, ~D edges)  tables ~D~n",
           [Pool, VTSize, IDG, IDGNodes, IDGEdges, NTables]),
    Tot = t(Tb, An, TS, CS, Cmp, Inc, Cpl, Z, One),
    format("answer tries ~D  clauses ~D (~D compiled)  answers ~D~n",
           [TS, CS, Cmp, An]),
    format("tables ~D: incremental ~D, complete ~D, 0 answers ~D, 1 answer ~D~n",
           [Tb, Inc, Cpl, Z, One]),
    statistics(heapused, Heap), statistics(atoms, Atoms), statistics(clauses, Clauses),
    statistics(atom_space, AS),
    format("heapused ~D  atoms ~D (~D B)  clauses ~D~n", [Heap, Atoms, AS, Clauses]),
    forall(st(K, N), format("status ~w: ~D~n", [K, N])),
    dict_pairs(Preds0, _, Pairs),
    map_list_to_pairs(pred_bytes, Pairs, Keyed),
    sort(1, @>=, Keyed, Sorted),
    format("~w~t~60|~t~w~12+~t~w~12+~t~w~16+~t~w~16+~t~w~12+~n",
           [predicate, tables, answers, 'trie B', 'clause B', inc]),
    forall(( nth1(I, Sorted, _-(_-(PI-p(Tb1,An1,TS1,CS1,_,Inc1,_,_,_)))), I =< Top ),
           format("~q~t~60|~t~D~12+~t~D~12+~t~D~16+~t~D~16+~t~D~12+~n",
                  [PI, Tb1, An1, TS1, CS1, Inc1])).

pred_bytes(_-(_-p(_,_,TS,CS,_,_,_,_,_)), B) :- B is TS+CS.

empty_nb_set_count(acc(_{}, t(0,0,0,0,0,0,0,0,0))).

add(Acc, M, V, T) :-
    functor(V, N, A),
    PI = M:N/A,
    '$trie_property'(T, value_count(Ans)),
    '$trie_property'(T, size(TS)),
    ( '$trie_property'(T, compiled_size(CS)) -> Cmp = 1 ; CS = 0, Cmp = 0 ),
    ( '$trie_property'(T, idg_size(_)) -> Inc = 1 ; Inc = 0 ),
    ( '$tbl_table_status'(T, St) -> true ; St = none ),
    ( St == complete -> Cpl = 1 ; Cpl = 0 ),
    status_count(St),
    ( Ans =:= 0 -> Z = 1 ; Z = 0 ),
    ( Ans =:= 1 -> One = 1 ; One = 0 ),
    Row = p(1, Ans, TS, CS, Cmp, Inc, Cpl, Z, One),
    arg(2, Acc, Tot), add_row(Tot, Row),
    arg(1, Acc, D0),
    term_to_atom(PI, Key),
    (   get_dict(Key, D0, P0-Old)
    ->  add_row(Old, Row), _ = P0
    ;   copy_term(Row, New),
        put_dict(Key, D0, PI-New, D1),
        nb_setarg(1, Acc, D1)
    ).

add_row(Acc, Row) :-
    forall(arg(I, Row, V), ( arg(I, Acc, V0), V1 is V0+V, nb_setarg(I, Acc, V1) )).

:- dynamic st/2.
status_count(St) :-
    ( atom(St) -> K = St ; functor(St, K, _) ),
    ( retract(st(K, N0)) -> N is N0+1 ; N = 1 ),
    assertz(st(K, N)).
