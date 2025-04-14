:- module(test_trie_attvars,
          [ test_trie_attvars/0,
            create_trie/2
          ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(terms)).
:- use_module(library(debug)).

test_trie_attvars :-
    run_tests([ trie_attvars
              ]).

has_trie_attvar_support :-
    trie_new(T),
    put_attr(X, x, t),
    catch(trie_insert(T, f(X)),
          error(type_error(free_of_attvar, _),_),
          fail).

% Execute all tests with trie_gen/2 and trie_gen_compiled/2.
term_expansion((test(Id,Attrs) :- Body),
               [ (test(Id,Attrs) :- Body),
                 (test(CId,Attrs) :- CBody)
               ]) :-
    mapsubterms(map_to_compiled, Body, CBody),
    CBody \== Body,
    atom_concat(Id, '_compiled', CId).

map_to_compiled(trie_gen(Trie, To),
                trie_gen_compiled(Trie, To)).

:- begin_tests(trie_attvars, [condition(has_trie_attvar_support)]).

test(simple, V == v) :-
    create_trie(T, [ f(_{a:v}) ]),
    trie_gen(T, f(Y)),
    get_attr(Y, a, V).
test(plain, V == v) :-
    create_trie(T, [ _{a:v} ]),
    trie_gen(T, Y),
    get_attr(Y, a, V).
test(nesting, V == v) :-
    create_trie(T, [ f(_{a:v}, a) ]),
    trie_gen(T, f(Y, Z)),
    assertion(Z == a),
    get_attr(Y, a, V).
test(nesting2, V == v) :-
    create_trie(T, [ f(X{a:v}, g(X,a)) ]),
    trie_gen(T, f(Y, Z)),
    assertion(Z == g(Y,a)),
    get_attr(Y, a, V).
test(shared, V == v) :-
    create_trie(T, [ f(X{a:v}, X) ]),
    trie_gen(T, f(Y, Z)),
    assertion(Y == Z),
    get_attr(Y, a, V).
test(unify_true, true) :-
    create_trie(T, [ f(X{call:freeze(X, even(X))}) ]),
    trie_gen(T, f(2)).
test(unify_fail_1, fail) :-
    create_trie(T, [ f(X{call:freeze(X, even(X))}) ]),
    trie_gen(T, f(1)).
test(unify_fail_2, fail) :-
    create_trie(T, [ f(X{call:freeze(X, even(X))}) ]),
    trie_gen(T, f(Y)),
    Y = 1.
test(unify_shared, Y == 2) :-
    create_trie(T, [ f(X{call:freeze(X, even(X))}, X) ]),
    trie_gen(T, f(2, Y)).
test(unify_shared, Y == 2) :-
    create_trie(T, [ f(X{call:freeze(X, even(X))}, X) ]),
    trie_gen(T, f(Y, 2)).
test(unify_shared, Z == z) :-
    create_trie(T, [ f(X{call:freeze(X, even(X))}, X) ]),
    freeze(B, Z=z),
    trie_gen(T, f(2, B)),
    assertion(B == 2).
test(unify_multi, true) :-
    create_trie(T, [ f(X{call:freeze(X, even(X))},
                       Y{call:freeze(Y, odd(Y))})
                   ]),
    trie_gen(T, f(2, 1)).

:- end_tests(trie_attvars).


create_trie(Trie, Values) :-
    trie_new(Trie),
    maplist(insert_value(Trie), Values).

insert_value(Trie, Value0) :-
    mapsubterms(to_attvar, Value0, Value),
    trie_insert(Trie, Value).

to_attvar(Dict, Var) :-
    is_dict(Dict, Var),
    var(Var),
    !,
    mapdict(add_attr(Var), Dict).

add_attr(_Var, call, Goal) =>
    call(Goal).
add_attr(Var, Key, Value) =>
    put_attr(Var, Key, Value).

even(X) :-
    X mod 2 =:= 0.

odd(X) :-
    X mod 2 =:= 1.
