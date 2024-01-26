/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
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

:- module(test_bomb,
          [ test_bomb/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(statistics)).

% library(time) implies we also have threads.
:- if(exists_source(library(time))).
:- use_module(library(time)).

/** <module> Test interrupts on possibly long running code

The so called "bomb" creates a term that  is small in physical size, but
exponential in the size involved  in   a  normal  depth-first left-right
_tree walk_. Several built-ins perform such a   tree walk. By creating a
bomb and calling one of these built-ins, the user can create a call that
cannot be interrupted and will run (practically) forever.

This can be exploited on services where   the  user can inject arbitrary
(safe) goals to create a DoS attack. The SWI-Prolog Pengines and derived
SWISH system a are notably sensitive  to   this.  On most services, user
cannot run arbitrary (safe) code on  the   server  and such services are
thus not affected.

A number of these  algorithms  process   terms  recursively  using  _raw
pointers_ and holding marks on  the  Prolog   terms.  We  cannot  run an
interrupt handler in this scenario because   the  callback on Prolog may
cause a garbage collect or stack shift.   For  such routines we make the
routine return with a special  code  on   a  signal,  undo all work done
sofar,  call  PL_handle_signals().  On  -1    (exception)  we  pass  the
exception, else we restart the algorithm from the beginning.
*/

test_bomb :-
    run_tests([ bomb_arith,
                bomb_vars,
                bomb_numbervars,
                bomb_compiler
              ],
              [ jobs(4)
              ]).

:- debug(bomb).

bomb(N, B) :-
    bomb(N, +, 1, B).

bomb(0, _, L, L) :-
    !.
bomb(N, F, L, T) :-
    T =.. [F,X,X],
    M is N-1,
    bomb(M, F, L, X).

test_bomb(X, Goal) :-
    test_bomb(+, 1, X, Goal).

%!  test_bomb(+F, +L, +X, +Goal) is semidet.
%
%   Run test_bomb_once/4 mas 10 times.  As   timing  is  involved, these
%   tests may fail. Retrying 10 times should   give  us a fair chance to
%   get through the test.

test_bomb(F, L, X, Goal) :-
    between(1, 10, _),
    test_bomb_once(F, L, X, Goal),
    !.

%!  test_bomb_once(+F, +L, +X, +Goal) is semidet.
%
%   Test the algorithm Goal once. This finds  a suitable bomb size, runs
%   the timeout test and interrupt test.

test_bomb_once(F, L, X, Goal) :-
    calibrate_bom_size(F, L, X, Goal, 20, Size, CPU),
    TimeLimit is CPU/(random(30)+4),
    timeout_bomb(Size, TimeLimit, F, L, X, Goal, TimeOut),
    (   TimeOut == true
    ->  true
    ;   debug(bomb, 'No timeout', []),
        fail
    ),
    (   signal_bomb(Size, TimeLimit, F, L, X, Goal)
    ->  true
    ;   debug(bomb, 'Signal test failed', []),
        fail
    ).

%!  calibrate_bom_size(+F, +L, +X, +Goal, +Size0, -Size, -CPU) is det.
%
%   Try to find a senible bomb size for  our test. This starts at 20 (1M
%   leafs) and increments/decrements until the   algorithm runs in about
%   0.1 seconds.

calibrate_bom_size(F, L, X, Goal, Size0, Size, CPU) :-
    call_time(timeout_bomb(Size0, 2, F, L, X, Goal, TimeOut), Time),
    CPU0 = Time.cpu,
    (   TimeOut == false,
        CPU0 < 0.05
    ->  Size1 is Size0+1,
        calibrate_bom_size(F, L, X, Goal, Size1, Size, CPU)
    ;   CPU0 > 0.2
    ->  Size1 is Size0-1,
        calibrate_bom_size(F, L, X, Goal, Size1, Size, CPU)
    ;   Size = Size0,
        CPU is CPU0
    ).

%!  timeout_bomb(+Size, +TimeLimit, +F, +L, +X, +Goal, -TimeOut) is det.
%
%   Create a bomb of Size as X  using   the  functor F and leaf L. Then,
%   call   Goal   on   the   bomb   and     try   to   stop   it   using
%   call_with_time_limit/2.

timeout_bomb(Size, TimeLimit, F, L, X, Goal, TimeOut) :-
    copy_term(X+Goal, X2+Goal2),
    bomb(Size, F, L, X2),
    catch(call_with_time_limit(TimeLimit, Goal2),
          time_limit_exceeded,
          TimeOut = true),
    ignore(TimeOut=false).

%!  signal_bomb(+Size, +TimeLimit, +F, +L, +X, +Goal) is semidet.
%
%   Create a bomb of Size as X  using   the  functor F and leaf L. Then,
%   call Goal on the bomb and send  a signal after TimeLimit. The signal
%   handler calls the garbage collector and   records that it was indeed
%   triggered.
%
%   This validates that the restart of the interrupted algorithm works.

:- thread_local
    gced/0.

signal_bomb(Size, TimeLimit, F, L, X, Goal) :-
    retractall(gced),
    copy_term(X+Goal, X2+Goal2),
    bomb(Size, F, L, X2),
    alarm(TimeLimit, sig_gc, _, [remove(true)]),
    call(Goal2),
    retract(gced).

sig_gc :-
    garbage_collect,
    assert(gced).

:- begin_tests(bomb_arith).

test(arith) :-
    test_bomb(X, _ is X).

:- end_tests(bomb_arith).

:- begin_tests(bomb_vars).

test(singleton) :-
    test_bomb(X, term_singletons(X, _)).
test(multiton) :-
    test_bomb(X, '$term_multitons'(X, _)).

:- end_tests(bomb_vars).

:- begin_tests(bomb_numbervars).

test(numbervars) :-
    test_bomb(X, numbervars(X, 0, _, [singletons(true)])).

:- end_tests(bomb_numbervars).

:- begin_tests(bomb_compiler).

test(conj) :-
    test_bomb(',', true, X, assert((t :- X))).
test(call, blocked("Runs out of C-stack")) :-
    test_bomb(',', true, X, call(X)).
test(arg) :-
    test_bomb(X, assert((t :- ground(X)))).

:- end_tests(bomb_compiler).

:- else.

test_bomb.

:- endif.
