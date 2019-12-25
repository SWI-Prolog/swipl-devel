:- module(test_dcg_high_order, [test_dcg_high_order/0]).
:- use_module(library(dcg/high_order)).

test_dcg_high_order :-
    run_tests([sequence]).

:- begin_tests(sequence).

a(a) --> "a".

b(b1) --> "b".
b(b2) --> "b".

sep --> ",".

start --> "(".

end --> ")".

test('sequence/2 ground list',
     forall(member(t(Codes, Result, Rest),
                   [t(``, [], []),
                    t(`x`, [], `x`),
                    t(`a`, [a], []),
                    t(`aaa`, [a,a,a], []),
                    t(`ax`, [a], `x`),
                    t(`aaax`, [a,a,a], `x`)
                   ]))
    ) :-
    phrase(sequence(a, Result), Codes, Rest).

test('sequence/2 order of solutions',
     all(List-Rest == [[a,a]-``, [a]-`a`, []-`aa`])) :-
    phrase(sequence(a, List), `aa`, Rest).

test('sequence/2 det element',
     [forall(member(t(Codes, Result, Rest),
                    [t(`a`, [a], []),
                     t(`x`, [], `x`),
                     t(`ax`, [a], `x`),
                     t(`aaa`, [a,a,a], ``),
                     t(`aaax`, [a,a,a], `x`)
                    ])),
      nondet
     ]) :-
    phrase(sequence(a, L), Codes, R),
    L == Result,
    R == Rest.

test('sequence/2 nondet element', all(B == [b1,b2])) :-
    phrase(sequence(b, [B,B]), `bb`, ``).
test('sequence/2 nondet element and rest', all(B == [b1,b2])) :-
    phrase(sequence(b, [B,B]), `bbx`, `x`).

test('sequence/3 det element',
     [forall(member(t(Codes, Result, Rest),
                    [t(``, [], []),
                     t(`a`, [a], []),
                     t(`x`, [], `x`),
                     t(`ax`, [a], `x`),
                     t(`a,a,a`, [a,a,a], ``),
                     t(`a,a,ax`, [a,a,a], `x`)
                    ])),
      nondet
     ]) :-
    phrase(sequence(a, sep, L), Codes, R),
    L == Result,
    R == Rest.

test('sequence/3 det element trailing sep', fixme('Sep should not be consumed')) :-
    phrase(sequence(a, sep, L), `a,`, R),
    L == [a],
    R == `,`.

test('sequence/3 det element trailing sep', fail) :-
    phrase(sequence(a, sep, L), `a,`, R),
    L == [a],
    R == [].

test('sequence/3 separator only', fail) :-
    phrase(sequence(a, sep, _L), `,`).

test('sequence/3 nondet element', fixme('Not sure what should happen')) :-
    phrase(sequence(b, sep, _L), `b,b`, _R).

test('sequence/5 det element',
     [forall(member(t(Codes, Result, Rest),
             [t(`()`, [], []),
              t(`(a)`, [a], []),
              t(`(a,a)`, [a,a], []),
              t(`()x`, [], `x`),
              t(`(a,a)x`, [a,a], `x`)
             ]))
     ]) :-
    phrase(sequence(start, a, sep, end, L), Codes, R),
    L == Result,
    R == Rest.

test('sequence/5 det element trailing sep', fail) :-
    phrase(sequence(start, a, sep, end, _L), `(a,)`, _R).

test('sequence/5 sep only', fail) :-
    phrase(sequence(start, a, sep, end, _L), `(,)`, _R).

test('sequence/5 nondet element') :-
    phrase(sequence(start, b, sep, end, L), `(b,b)`, R),
    L == [b1,b1],
    R == [].

test('sequence/5 nondet element, not first solution', fail) :-
    phrase(sequence(start, b, sep, end, L), `(b,b)`),
    L \== [b1,b1].

:- end_tests(sequence).
