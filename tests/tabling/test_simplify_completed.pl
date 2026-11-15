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


:- module(test_simplify_completed,
          [ test_simplify_completed/0
          ]).
:- use_module(library(plunit)).

/** <module> Simplification reaching a table that completed in an older SCC

Making an answer  unconditional propagates along the delay  lists, and the
answers reached that way may live in  tables that completed in an earlier
SCC.  The worklist of such a table  outlives its component, so it may not
be used to reach the component.

Without  the fix,  the  propagation  below writes  to  the freed
component. That is invisible on an ordinary build; run this  test under
AddressSanitizer or valgrind to see it.
*/

test_simplify_completed :-
    run_tests([ simplify_completed
              ]).

:- begin_tests(simplify_completed).

:- dynamic d/1 as incremental.
:- dynamic blk/1 as incremental.
:- table x/1 as incremental.
:- table y/1 as incremental.
:- table o/1.                   % plain: completes and is not re-evaluated
:- table a/1.

x(X) :- d(X), tnot(y(X)).
y(X) :- d(X), tnot(x(X)), \+ blk(X).
o(X) :- x(X).                   % delays on x/1's conditional answer
a(X) :- o(X).                   % ... and o/1 has a dependent of its own

conditions(Variant, Conditions) :-
    findall(Return-Kind,
            ( current_table(Variant, Trie),
              '$tbl_answer'(Trie, Return, Cond),
              (   Cond == true
              ->  Kind = true
              ;   Kind = conditional
              )
            ),
            Conditions).

test(propagate_into_completed_table,
     [ X == [ret(1)-true], O == [ret(1)-true], A == [ret(1)-true] ]) :-
    abolish_all_tables,
    retractall(d(_)), retractall(blk(_)),
    assertz(d(1)),
    %  x(1) and y(1) are a negative loop, so all three tables complete with
    %  a conditional answer.  o/1 and a/1 then complete in their own SCCs.
    forall(x(_), true),
    forall(o(_), true),
    forall(a(_), true),
    conditions(x(_), [ret(1)-conditional]),
    conditions(o(_), [ret(1)-conditional]),
    conditions(a(_), [ret(1)-conditional]),
    %  blk(1) makes y(1) false, so the re-evaluation of x/1 derives its
    %  answer unconditionally.  That propagates through o/1 into a/1.
    assertz(blk(1)),
    forall(x(_), true),
    conditions(x(_), X),
    conditions(o(_), O),
    conditions(a(_), A).

:- end_tests(simplify_completed).
