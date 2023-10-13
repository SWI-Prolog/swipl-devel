/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
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

:- module(test_agc_copyterm,
          [ test_agc_copyterm/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_agc_copyterm :-
    run_tests([ agc_copyterm
              ]).

:- begin_tests(agc_copyterm).

test(copy) :-
    go(3).

go(0) :-
    !.
go(N) :-
    functor(Result, a, 1000),
    (   between(1, 1000, I),
        functor(Term, t, 100),
        set_atoms(1, 100, I, Term),
        nb_setarg(I, Result, Term),
        fail
    ;   check_result(Result, 1, 1000)
    ),
    N2 is N - 1,
    go(N2).

set_atoms(From, To, I, Term) :-
    From =< To,
    !,
    atomic_list_concat([a,I,From], '_', Atom),
    arg(From, Term, Atom),
    F2 is From+1,
    set_atoms(F2, To, I, Term).
set_atoms(_, _, _, _).


check_result(Result, From, To) :-
    forall(between(From, To, I),
           (   arg(I, Result, T),
               check_term(T, 1, 100, I)
           )).

check_term(Result, From, To, C) :-
    between(From, To, I),
    arg(I, Result, Atom),
    atomics_to_string([a,C,From], '_', String),
    assertion(atom_string(Atom, String)).

:- end_tests(agc_copyterm).
