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


:- module(test_gc_live_vars,
          [ test_gc_live_vars/0
          ]).
:- use_module(library(plunit)).

/** <module> GC must keep variables that the VM reads after a call

When GC runs inside a call, it walks the rest of the clause to find the
environment variables that are still used.  Variables it considers dead
are reset.  Each test binds a variable, calls garbage_collect/0 and then
runs a VM instruction that reads the variable.
*/

test_gc_live_vars :-
    run_tests([ gc_live_vars ]).

:- begin_tests(gc_live_vars).

test(rational) :-
    X is 1 rdiv 3, garbage_collect, rational(X).
test(integer) :-
    X is 2**70, garbage_collect, integer(X).
test(float) :-
    X is 2.0**0.5, garbage_collect, float(X).
test(number) :-
    X is 2**70, garbage_collect, number(X).
test(atomic) :-
    string_concat(a, b, X), garbage_collect, atomic(X).
test(atom) :-
    atom_concat(a, b, X), garbage_collect, atom(X).
test(string) :-
    string_concat(a, b, X), garbage_collect, string(X).
test(compound) :-
    copy_term(f(_), X), garbage_collect, compound(X).
test(callable) :-
    copy_term(f(_), X), garbage_collect, callable(X).
test(var) :-
    copy_term(_, X), garbage_collect, var(X).
test(roundtoward, Y == 0.5) :-
    roundtoward_after_gc(Y).

:- end_tests(gc_live_vars).

%   Optimised to compile the variable rounding mode to A_ROUNDTOWARDS_V.
%   plunit does not load tests if optimise is true.

:- set_prolog_flag(optimise, true).

roundtoward_after_gc(Y) :-
    atom_concat(to_, nearest, Mode),
    X = 1.0,
    garbage_collect,
    Y is roundtoward(X/2, Mode).

:- set_prolog_flag(optimise, false).
