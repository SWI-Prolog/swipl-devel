/* As ergo_bench.pl, but records the calls to ergoAIp's rule metadata as
   abstract(0) dependencies.  See ../ergoai-cyc150.md, proposal 1.
*/
:- consult(ergo_bench).

abstract_preds(['$opposes_f'/2, '$opposes4_f'/4, '$opposes_open'/2, '$cand'/2]).

make_abstract :-
    abstract_preds(PIs),
    forall(( member(N/A, PIs),
             functor(H, N, A),
             current_predicate(_, M:H),
             \+ predicate_property(M:H, imported_from(_)),
             predicate_property(M:H, incremental)
           ),
           ( dynamic((M:N/A) as (incremental, abstract(0))),
             format(user_error, "abstract: ~q~n", [M:N/A]) )).

bench_abs :-
    setup,
    A = 'kbq-cpttest-diploidnumberfordogs-2.flr',
    B = 'kbq-cpttest-whatkindofthingisahaploidcell-4.flr',
    load_base(A),
    make_abstract,
    add_case(A), solve('solve A (cold)'),
    report(after_A),
    solve('solve A (again)'),
    add_case(B), solve('solve B (tables kept)'),
    report(after_B),
    wall(abolish, abolish_all_tables),
    wall(agc, agc_settled(600)),
    report(after_abolish).
