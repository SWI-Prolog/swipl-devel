/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(test_reeval_exceptions,
          [ test_reeval_exceptions/0
          ]).
:- use_module(library(debug)).

/** <module> Test exceptions during re-evaluation

This test deals with exceptions  during   reevaluation.  If an exception
happens during evaluation of a normal tabled   goal the SCC is abandoned
and all tables in the SCC are   abolished. If the SCC contains _invalid_
incremental tables these are reset to the same invalid state as they had
before the re-evaluation started. This  implies that their re-evaluation
can be restarted, either by this thread or, when using shared tables, by
another thread.
*/

:- table (p/1, q/1) as incremental.
:- dynamic([d/1], [incremental(true)]).
:- dynamic do_ex/1, reevalled/1.

:- meta_predicate
    all(?, 0, +),
    all(?, 0, +, +),
    evaluated(0, -).

p(X) :- reeval(p/1), q(X).
q(X) :- reeval(q/1), d(X), ex, X < 10.

ex :- do_ex(Ex), !, throw(Ex).
ex.

test_reeval_exceptions :-
    retractall(d(_)),
    abolish_all_tables,
				all(X, p(X), []),
    assert(d(1)),		all(X, p(X), [1], [p/1, q/1]),
    assert(d(11)),		all(X, p(X), [1], [q/1]),

    assert(do_ex(sorry)),     % full reevaluation after exception
    assert(d(2)),
    catch(p(_), E, true),	assertion(E == sorry),
    retractall(do_ex(_)),	all(X, p(X), [1,2], [p/1, q/1]),

    assert(do_ex(sorry)),     % partial reevaluation after exception
    assert(d(12)),
    catch(p(_), E, true),	assertion(E == sorry),
    retractall(do_ex(_)),	all(X, p(X), [1,2], [q/1]),

    true.

%!  all(?Template, :Goal, +Expected) is semidet.
%!  all(?Template, :Goal, +Expected, +ExpectEval) is semidet.
%
%   Find all instantiations of Template wrt. Goal and verify this is the
%   same set as Expected.  The  all/4   variant  verifies  that only the
%   specified predicates are re-evaluated.

all(X, G, Expected) :-
    findall(X, G, Got),
    expected(result, Expected, Got).

all(X, G, Expected, Evaluated) :-
    evaluated(findall(X, G, Got), EvaluatedGot),
    expected(result, Expected, Got),
    expected(reeval, Evaluated, EvaluatedGot).

expected(Id, Expected, Got) :-
    sort(Expected, Expected1),
    sort(Got, Got1),
    (   Got1 =@= Expected1
    ->  true
    ;   format(user_error, '~w: expected ~p, got ~p~n', [Id, Expected1, Got1]),
        fail
    ).

evaluated(G, L) :-
    retractall(reevalled(_)),
    G,
    findall(X, reevalled(X), L0),
    sort(L0, L).

reeval(T) :-
    (   reevalled(T)
    ->  true
    ;   asserta(reevalled(T))
    ).
