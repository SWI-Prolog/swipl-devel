:- module(test_trie_index,
          [ test_trie_index/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(random)).

% trie_gen/2,3 prunes its walk on the bound parts of the query, also
% after a variable in the query or in the trie.  These tests compare its
% answers with those of enumerating every key and unifying it with the
% query, over generated and hand-picked keys and queries, and check that
% the walk survives garbage collection, stack shifts and changes to the
% trie between answers.

test_trie_index :- run_tests(trie_index).

:- begin_tests(trie_index).

test(generated_unification) :-
    set_random(seed(904219)),
    forall(between(1, 20, _),
           setup_call_cleanup(trie_new(T), generated(T), trie_destroy(T))).

test(cyclic_query, Ids == [1, 2]) :-
    setup_call_cleanup(trie_new(T),
        (trie_insert(T, p(_, a), 1), trie_insert(T, p(f(_), a), 2),
         X = f(X), findall(I, trie_gen(T, p(X, a), I), Found), sort(Found, Ids)),
        trie_destroy(T)).

test(deleted_pending_answer, true(Count == 1)) :-
    setup_call_cleanup(trie_new(T),
        (forall(between(1, 10, I), trie_insert(T, p(I, 17))),
         findall(I, (trie_gen(T, p(I, 17)),
                     forall((between(1, 10, J), J =\= I),
                            ignore(trie_delete(T, p(J, 17), _)))), Ids),
         length(Ids, Count)),
        trie_destroy(T)).

test(garbage_collection_between_answers, true(Count == 100)) :-
    setup_call_cleanup(trie_new(T),
        (forall(between(1, 100, I), trie_insert(T, p(I, f(a), _))),
         findall(I, (trie_gen(T, p(I, f(a), b)), garbage_collect), Ids),
         length(Ids, Count)), trie_destroy(T)).

test(variable_mask_overflow, true(Ids == [1, 2])) :-
    length(Vars, 40), Prefix =.. [f|Vars], last(Vars, Last),
    setup_call_cleanup(trie_new(T),
        (trie_insert(T, p(Prefix, Last, 13), 1),
         trie_insert(T, p(Prefix, a, 13), 2),
         findall(I, trie_gen(T, p(_, a, 13), I), Found), sort(Found, Ids)),
        trie_destroy(T)).

test(attributed_query, true(Ids == [2])) :-
    setup_call_cleanup(trie_new(T),
        (trie_insert(T, p(1, b), 1), trie_insert(T, p(2, b), 2),
         freeze(X, X =:= 2), findall(I, trie_gen(T, p(X, b), I), Ids)),
        trie_destroy(T)).

% Compounds that end before their last argument close with POP keys, and
% last-argument chains share one: skips must end inside them.
test(compound_pop) :-
    Keys = [ p(f(g(a), b), c), p(f(g(a, x), b), c), p(f(_, b), c), p(_, c),
	     p(f(g(a)), c), p(f(g(h(a))), d), p(a, f(g(h(a)))),
	     p(f(g(a), b), f(c)), p(f(z(), b), c), p([a, b, c], d),
	     p([a, b|_], d), p([a|_], e), p(g(f(a), _), [x, y]),
	     p(g(f(a, b), h(c, d)), e(f)), p(z(), z()) ],
    Queries = [ p(_, c), p(f(_, b), c), p(f(g(_), _), c), p(f(g(a), b), _),
		p(_, _), p(f(_), c), p(a, f(g(h(_)))), p(a, f(_)),
		p(f(g(h(a))), _), p(f(z(), _), c), p([a, b, c], _),
		p([a, _, c], d), p([_|_], _), p(g(_, b), [x|_]),
		p(f(g(_, x), _), c), p(g(f(_, b), _), e(_)),
		p(g(_, h(c, _)), _), p(z(), _), p(_, z()) ],
    setup_call_cleanup(trie_new(T),
        ( insert_keys(Keys, T, 1, Pairs),
          forall(member(Query, Queries), same_answers(T, Pairs, Query)) ),
        trie_destroy(T)).

test(repeated_variables) :-
    Keys = [ p(a, b), p(a, a), p(X, X), p(_, _), p(f(Y), Y), p(f(a), b),
	     p(f(Z), g(Z)), p(g(W, W), W), p(h(U, U, V), V) ],
    Queries = [ p(S, S), p(a, _), p(f(R), R), p(f(_), a), p(_, b),
		p(f(P), g(P)), p(f(b), g(c)), p(g(O, O), a), p(g(a, b), _),
		p(h(a, N, N), b), p(h(M, L, L), M) ],
    setup_call_cleanup(trie_new(T),
        ( insert_keys(Keys, T, 1, Pairs),
          forall(member(Query, Queries), same_answers(T, Pairs, Query)) ),
        trie_destroy(T)).

% A stored attributed variable beside constants in a hashed node is
% reached by a bound query; its hook accepts or rejects the binding.
test(stored_attvar_beside_constants) :-
    forall(member(Mode-Expected, [accept-[1, 4], reject-[4]]),
           stored_attvar(Mode, Expected)).

% A bound value the trie never interned matches only stored variables.
test(absent_indirect_value) :-
    setup_call_cleanup(trie_new(T),
        ( trie_insert(T, p(_, 1), 1), trie_insert(T, p("text", 2), 2),
          trie_insert(T, p(12345678901234567890, 3), 3),
          trie_insert(T, p(2.5, 4), 4),
          trie_insert(T, q("a", 1), 5), trie_insert(T, q("b", 2), 6),
          forall(member(Query-Expected,
                        [ p("other", _)-[1], p("text", _)-[1, 2],
                          p(98765432109876543210, _)-[1],
                          p(12345678901234567890, _)-[1, 3],
                          p(3.5, _)-[1], p(2.5, _)-[1, 4],
                          q("c", _)-[], q("a", _)-[5] ]),
                 ( findall(V, trie_gen(T, Query, V), Vs), msort(Vs, Sorted),
                   assertion(Sorted == Expected) )) ),
        trie_destroy(T)),
    setup_call_cleanup(trie_new(T2),
        ( trie_insert(T2, p(_, a), 1),
          findall(V, trie_gen(T2, p("s", a), V), Vs2),
          assertion(Vs2 == [1]) ),
        trie_destroy(T2)).

% Collections and stack shifts move the query between answers; the
% cursor is rebuilt rather than kept.
test(stack_shift_between_answers) :-
    statistics(global_shifts, S0),
    statistics(garbage_collection, [G0|_]),
    setup_call_cleanup(trie_new(T),
        ( forall(between(1, 30, I),
                 ( trie_insert(T, p(f(I), g(_, b)), I),
                   trie_insert(T, p(f(I), g(_, c)), c(I)) )),
          Query = p(f(_), g(h(1), b)),
          findall(V, (trie_gen(T, Query, V), churn), Found0), msort(Found0, Found),
          findall(V, (trie_gen(T, K, V), K = Query), Expected0),
          msort(Expected0, Expected),
          assertion(Found == Expected), assertion(length(Found, 30)) ),
        trie_destroy(T)),
    statistics(global_shifts, S1),
    statistics(garbage_collection, [G1|_]),
    assertion(S1 > S0), assertion(G1 > G0).

% Keys inserted while answers are pending, into the enumerated table and
% beside the path, never repeat an answer or lose one that was there.
test(insert_between_answers) :-
    setup_call_cleanup(trie_new(T),
        ( forall(between(1, 50, I), trie_insert(T, p(_, I), I)),
          findall(V, ( trie_gen(T, p(a, _), V),
                       ( integer(V), V =< 50
                       -> trie_insert(T, p(b, V), b(V)),
                          trie_insert(T, q(V), q(V)),
                          N is 1000+V, trie_insert(T, p(_, N), N)
                       ;  true ) ),
                  Vs),
          partition([E]>>(E =< 50), Vs, Early, Late),
          msort(Early, Sorted), numlist(1, 50, All),
          assertion(Sorted == All),
          msort(Late, LateSorted), sort(Late, LateSet),
          assertion(LateSorted == LateSet),
          forall(member(X, Late), assertion(X > 1000)) ),
        trie_destroy(T)).

test(delete_between_answers, true(Count == 1)) :-
    setup_call_cleanup(trie_new(T),
        ( forall(between(1, 50, I), trie_insert(T, p(_, I), I)),
          findall(V, ( trie_gen(T, p(a, _), V),
                       forall(between(1, 50, J),
                              ignore(trie_delete(T, p(_, J), _))) ),
                  Vs),
          length(Vs, Count) ),
        trie_destroy(T)).

% Below a wildcard, the bound argument is looked up rather than every
% stored value tried, so a single match leaves no choice point.  Det is
% tested in the body, as plunit cuts a choice point the body leaves.
test(single_match_is_deterministic, V == 50) :-
    setup_call_cleanup(trie_new(T),
        ( forall(between(1, 100, I), trie_insert(T, p(_, I), I)),
          call_cleanup(trie_gen(T, p(a, 50), V), Det = true),
          assertion(Det == true) ),
        trie_destroy(T)).

% After an answer below a pruning choice the search stops and leaves the
% rest to the redo; a cut discards it instead.
test(cut_after_pruned_answer) :-
    setup_call_cleanup(trie_new(T),
        ( forall(between(1, 50, I),
                 ( trie_insert(T, p(I, a), I), trie_insert(T, p(I, b), b(I)) )),
          forall(between(1, 3, N),
                 ( findall(V, limit(N, trie_gen(T, p(_, a), V)), Vs),
                   assertion(length(Vs, N)),
                   assertion(maplist(integer, Vs)) )) ),
        trie_destroy(T)).

% A bound Value rejects candidates after Key unified with them, so the
% search goes on with Key unbound again.
test(bound_value, Xs == [25]) :-
    setup_call_cleanup(trie_new(T),
        ( forall(between(1, 50, I),
                 ( trie_insert(T, p(I, a), I), trie_insert(T, p(I, b), b(I)) )),
          findall(X, trie_gen(T, p(X, a), 25), Xs) ),
        trie_destroy(T)).

% Deeper keys with lists, zero-argument compounds, rationals and signed
% zeros, queried by generalisations and instances of the stored keys.
test(generated_shapes) :-
    set_random(seed(310877)),
    forall(between(1, 10, _),
           setup_call_cleanup(trie_new(T), generated_shapes(T), trie_destroy(T))).

:- end_tests(trie_index).

stored_attvar(Mode, Expected) :-
    setup_call_cleanup(trie_new(T),
        ( put_attr(X, test_trie_index, Mode), trie_insert(T, p(X), 1),
          trie_insert(T, p(b), 2), trie_insert(T, p(c), 3),
          trie_insert(T, p(_), 4),
          put_attr(Y, test_trie_index, Mode), trie_insert(T, q(Y, Y), 5),
          trie_insert(T, q(a, b), 6), trie_insert(T, q(_, b), 7),
          findall(V, trie_gen(T, p(a), V), Found0), msort(Found0, Found),
          assertion(Found == Expected),
          forall(member(Query, [p(a), p(b), p(_), q(a, a), q(a, b), q(_, b),
				q(f(_), _)]),
                 unified_answers(T, Query)) ),
        trie_destroy(T)).

% The hook of the attributes stored_attvar/2 puts: `accept` admits any
% binding and `reject` none.
attr_unify_hook(Mode, _) :-
    Mode == accept.

% Allocate past the global stack, collect and trim, so the query moves.
churn :-
    numlist(1, 5000, L), sum_list(L, _),
    garbage_collect, trim_stacks.

generated_shapes(T) :-
    findall(K, (between(1, 150, _), rich(K)), Keys),
    insert_keys(Keys, T, 1, Pairs),
    forall(( member(K-_, Pairs), copy_term(K, C),
             ( generalize(C, Q) ; specialize(C, Q) ) ),
           same_answers(T, Pairs, Q)),
    forall((between(1, 100, _), rich(Q)), same_answers(T, Pairs, Q)).

rich(T) :- length(Vars, 5), rich(4, Vars, T).
rich(Depth, Vars, T) :-
    random_between(0, 11, Kind),
    (   Kind < 2 -> random_member(T, Vars)
    ;   Kind < 4 -> random_member(T, [a, b, [], 0, 1, -1, 2.5, -0.0, 0.0,
				      "s", "", 98765432109876543210, 1r3])
    ;   Depth =:= 0 -> random_member(T, [a, b, []])
    ;   Next is Depth-1,
        random_member(F/A, [f/1, g/2, h/3, '[|]'/2, z/0, k/5]),
        length(Args, A), maplist(rich(Next, Vars), Args),
        compound_name_arguments(T, F, Args)
    ).

% Replace random subterms by fresh variables.
generalize(T, G) :-
    (   var(T) -> G = T
    ;   random(R), R < 0.25 -> true
    ;   compound(T)
    ->  compound_name_arguments(T, N, As), maplist(generalize, As, Gs),
        compound_name_arguments(G, N, Gs)
    ;   G = T
    ).

% Bind random variables to random terms.  Not forall/2, which would undo
% the bindings.
specialize(T, T) :-
    term_variables(T, Vs),
    maplist(maybe_bind, Vs).

maybe_bind(V) :-
    (   random(R), R < 0.5
    ->  rich(2, [_], V)
    ;   true
    ).

unified_answers(T, Query) :-
    findall(V, trie_gen(T, Query, V), Found0), msort(Found0, Found),
    findall(V, (trie_gen(T, K, V), K = Query), Expected0),
    msort(Expected0, Expected),
    assertion(Found == Expected).

generated(T) :-
    findall(K, (between(1, 100, _), term(K)), Keys),
    insert_keys(Keys, T, 1, Pairs),
    forall((member(K-_, Pairs), copy_term(K, Q)), same_answers(T, Pairs, Q)),
    forall((between(1, 100, _), term(Q)), same_answers(T, Pairs, Q)).

insert_keys([], _, _, []).
insert_keys([K|Ks], T, I, Pairs) :-
    (trie_lookup(T, K, _) -> Pairs = Rest
    ; trie_insert(T, K, I), Pairs = [K-I|Rest]),
    Next is I+1, insert_keys(Ks, T, Next, Rest).

same_answers(T, Pairs, Q) :-
    findall(I-Answer, (member(K-I, Pairs), copy_term(Q-K, Answer-Copy),
                      Answer = Copy, numbervars(Answer, 0, _)), Expected0),
    findall(I-Q, (trie_gen(T, Q, I), numbervars(Q, 0, _)), Actual0),
    sort(Expected0, Expected), sort(Actual0, Actual),
    assertion(Actual == Expected).

term(T) :- length(Vars, 4), term(3, Vars, T).
term(Depth, Vars, T) :-
    random_between(0, 8, Kind),
    (Kind < 2 -> random_member(T, Vars)
    ; Kind =:= 2 -> random_member(T, [a,b,c,0,1,2,3.5,"text",12345678901234567890])
    ; Depth =:= 0 -> random_member(T, [a,b,c])
    ; Next is Depth-1, random_member(F/Arity, [f/1,f/2,g/2,p/3,':'/2]),
      length(Args, Arity), maplist(term(Next, Vars), Args), T =.. [F|Args]).
