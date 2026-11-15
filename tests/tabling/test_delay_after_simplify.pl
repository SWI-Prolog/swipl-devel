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


:- module(test_delay_after_simplify,
          [ test_delay_after_simplify/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(wfs)).

/** <module> Conditions reported for answers simplified after completion

A completed table answers from its compiled clause (trie_gen_compiled/2),
into which  compile_trie_node() writes a  T_DELAY instruction for  every
answer that  is conditional at  that moment.  Simplification can  make
such  an  answer unconditional  without  changing  the trie,  which
leaves the  compiled clause -- and hence  call_delays/2 -- reporting a
condition that no longer exists.
*/

test_delay_after_simplify :-
    run_tests([ delay_after_simplify
              ]).

:- begin_tests(delay_after_simplify).

:- dynamic d/1 as incremental.
:- dynamic blk/1 as incremental.
:- table x/1 as incremental.
:- table y/1 as incremental.
:- table o/1.                   % plain: completed and compiled, never re-evaluated

x(X) :- d(X), tnot(y(X)).
y(X) :- d(X), tnot(x(X)), \+ blk(X).
o(X) :- x(X).

%  Truth of o/1 as call_delays/2 sees it and as the table has it.  The two
%  must agree.

reported(Truths) :-
    findall(X-T,
            ( call_delays(o(X), Delays),
              (   Delays == true
              ->  T = true
              ;   T = conditional
              )
            ),
            Truths).

stored(Truths) :-
    findall(X-T,
            ( current_table(o(_), Trie),
              '$tbl_answer'(Trie, Return, Cond),
              arg(1, Return, X),
              (   Cond == true
              ->  T = true
              ;   T = conditional
              )
            ),
            Truths).

setup_conditional :-
    abolish_all_tables,
    retractall(d(_)), retractall(blk(_)),
    assertz(d(1)),
    forall(x(_), true),
    forall(o(_), true).

%  x(1) and y(1) are a negative loop, so o/1's answer is conditional.  It
%  stays that way, and both views must say so.

test(conditional, [ Reported == [1-conditional], Stored == [1-conditional] ]) :-
    setup_conditional,
    reported(Reported),
    stored(Stored).

%  blk(1) makes y(1) false and x(1) unconditionally true.  o/1 is not
%  re-evaluated: its answer is made unconditional by the simplification.

test(simplified, [ Reported == [1-true], Stored == [1-true] ]) :-
    setup_conditional,
    reported(_),                % completes and compiles o/1's trie
    assertz(blk(1)),
    forall(x(_), true),
    stored(Stored),
    reported(Reported).

:- end_tests(delay_after_simplify).
