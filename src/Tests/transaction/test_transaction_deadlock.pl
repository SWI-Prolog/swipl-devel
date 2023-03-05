/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(test_transaction_deadlock,
          [ test_transaction_deadlock/0
          ]).
:- use_module(library(plunit)).

test_transaction_deadlock :-
    run_tests([ transaction_deadlock
	      ]).

:- begin_tests(transaction_deadlock,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(deadlock) :-
    dl_loop(1000).

:- dynamic p/1, q/1, done/0.

%!  dl_loop(N) is det.
%
%   Test  mutual deadlock  between L_PREDICATE  and L_GENERATION  that
%   could occur because comitting a transaction locks L_GENERATION and
%   then  updates  the  clauses   which  may  lock  L_PREDICATE  while
%   e.g.  abolish/1 locks  L_PREDICATE and  may update  the generation
%   when clauses are removed.
%
%   We now  move actual removing  of the clauses during  a transaction
%   commit to outside the L_GENERATION locked area.

dl_loop(N) :-
    retractall(done),
    thread_create(tr_on_p, Id, []),
    loop_on_q(N),
    thread_join(Id).

tr_on_p :-
    done,
    !.
tr_on_p :-
    assert(p(1)),
    transaction(retract(p(1))),
    tr_on_p.

loop_on_q(0) :-
    !,
    asserta(done).
loop_on_q(N) :-
    assert(q(aap)),
    abolish(q/1),
    N2 is N - 1,
    loop_on_q(N2).

:- end_tests(transaction_deadlock).
