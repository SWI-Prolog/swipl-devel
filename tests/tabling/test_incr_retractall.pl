/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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


:- module(test_incr_retractall,
          [ test_incr_retractall/0
          ]).
:- use_module(library(plunit)).

/** <module> Updates refused by the incremental tabling guard

An update of an incremental dynamic predicate that would invalidate the
table  that is  being evaluated  is  refused with  a permission_error  by
'$idg_changed'/1.  This must  be reported  to the  caller whatever  shape
the update has.  retractall/1 with an unbound  argument used to return
`true` with  the exception  still pending, retracting  nothing: the
caller saw a successful retractall over a predicate that still held all
its clauses.
*/

test_incr_retractall :-
    run_tests([ incr_update_refused
              ]).

:- begin_tests(incr_update_refused).

:- dynamic f/1 as incremental.
:- table t/1 as incremental.

t(X) :- f(X), update.

:- dynamic update_goal/1.

update :-
    update_goal(G),
    call(G).

%!  refused(+Update, -Error) is semidet.
%
%   Run Update from inside the evaluation of t/1 and return the error it
%   raises.  Fails if the update is not refused.

refused(Update, Error) :-
    retractall(f(_)),
    forall(member(X, [1,2,3]), assertz(f(X))),
    retractall(update_goal(_)),
    assertz(update_goal(Update)),
    abolish_all_tables,
    catch(forall(t(_), true), Error, true),
    nonvar(Error).

facts(Fs) :-
    findall(X, f(X), Fs).

test(retractall_var, Error = error(permission_error(update,variant,_),_)) :-
    refused(retractall(f(_)), Error),
    facts([1,2,3]).
test(retractall_nonvar, Error = error(permission_error(update,variant,_),_)) :-
    refused(retractall(f(1)), Error),
    facts([1,2,3]).
test(retract, Error = error(permission_error(update,variant,_),_)) :-
    refused(retract(f(1)), Error),
    facts([1,2,3]).
test(assert, Error = error(permission_error(update,variant,_),_)) :-
    refused(assertz(f(4)), Error),
    facts([1,2,3]).

%  retractall/1 that is not refused must still empty the predicate

test(retractall_outside_evaluation, Fs == []) :-
    retractall(f(_)),
    forall(member(X, [1,2,3]), assertz(f(X))),
    abolish_all_tables,
    retractall(f(_)),
    findall(X, f(X), Fs).

:- end_tests(incr_update_refused).
