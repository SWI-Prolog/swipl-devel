/*  Part of SWI-Prolog

    Author:        Boris Vassilev
    E-mail:        boris.vassilev@gmail.com
    WWW:           http://www.swi-prolog.org

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

:- use_module(library(dcg/basics), [string//1]).

test('sequence//2 ground list',
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

test('sequence//2 order of solutions',
     all(List-Rest == [[a,a]-``, [a]-`a`, []-`aa`])) :-
    phrase(sequence(a, List), `aa`, Rest).

test('sequence//2 det element',
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

test('sequence//2 nondet element', all(B == [b1,b2])) :-
    phrase(sequence(b, [B,B]), `bb`, ``).
test('sequence//2 nondet element and rest', all(B == [b1,b2])) :-
    phrase(sequence(b, [B,B]), `bbx`, `x`).

test('sequence//3 det element',
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

test('sequence//3 det element trailing sep', fail) :-
    phrase(sequence(a, sep, L), `a,`, R),
    L == [a],
    R == `,`.

test('sequence//3 det element trailing sep consumed silently', fail) :-
    phrase(sequence(a, sep, L), `a,`, R),
    L == [a],
    R == [].

test('sequence//3 separator only', fail) :-
    phrase(sequence(a, sep, _L), `,`).

test('sequence//3 nondet element', nondet) :-
    phrase(sequence(string, sep, L), `b,b`, R),
    L == [`b`, `b`],
    R == [].

test('sequence//5 det element',
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

test('sequence//5 det element trailing sep', fail) :-
    phrase(sequence(start, a, sep, end, _L), `(a,)`, _R).

test('sequence//5 sep only', fail) :-
    phrase(sequence(start, a, sep, end, _L), `(,)`, _R).

test('sequence//5 nondet element') :-
    phrase(sequence(start, string, sep, end, L), `(b,b)`, R),
    L == [`b`, `b`],
    R == [].

:- end_tests(sequence).
