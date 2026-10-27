/* cyc150 benchmark for the abolish and compile changes.
   cd ~/src/prolog/ErgoAI && swipl --stack-limit=16g -g bench -t halt <this file>
   Reports solve times, compiled clauses, atoms, table space, RSS and the
   time to abolish all tables.  See ../ergoai-cyc150.md.
*/
:- consult(ergo_workflow).

clauses(N) :-
    predicate_property(system:trie_gen_compiled(_,_), number_of_clauses(N)), !.
clauses(0).

rss(Key, KB) :-
    read_file_to_string('/proc/self/status', S, []),
    split_string(S, "\n", "", Ls),
    member(L, Ls), sub_string(L, 0, _, _, Key), !,
    split_string(L, " \t:", " \t:", Parts), exclude(==(""), Parts, [_,V|_]),
    number_string(KB, V).

report(Label) :-
    clauses(N), rss("VmRSS", R), rss("VmHWM", H),
    statistics(table_space_used, TS), statistics(atoms, A),
    format(user_error, "~w: ~D compiled clauses, ~D atoms, table space ~D, RSS ~D MB, peak ~D MB~n",
           [Label, N, A, TS, R//1024, H//1024]).

wall(Label, G) :-
    get_time(T0), statistics(cputime, C0),
    call(G),
    get_time(T1), statistics(cputime, C1),
    W is T1-T0, C is C1-C0,
    format(user_error, "~w: wall ~2f s, cpu ~2f s~n", [Label, W, C]).

bench :-
    setup,
    A = 'kbq-cpttest-diploidnumberfordogs-2.flr',
    B = 'kbq-cpttest-whatkindofthingisahaploidcell-4.flr',
    load_base(A),
    add_case(A), solve('solve A (cold)'),
    report(after_A),
    solve('solve A (again)'),
    add_case(B), solve('solve B (tables kept)'),
    report(after_B),
    wall(abolish, abolish_all_tables),
    wall(agc, agc_settled(600)),
    report(after_abolish).

%   Wait (at most N tenths of a second) until atom-GC freed the tries.
agc_settled(N) :-
    garbage_collect_atoms,
    statistics(table_space_used, TS),
    (   TS < 10 000 000
    ->  true
    ;   N > 0
    ->  sleep(0.1), N2 is N-1, agc_settled(N2)
    ;   format(user_error, "AGC did not settle~n", [])
    ).
