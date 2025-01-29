/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2024, SWI-Prolog Solutions b.v.
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

:- module(test_thread_signals,
	  [ test_thread_signals/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(prolog_stack)).
:- use_module(library(ansi_term)).
:- use_module(library(lists)).
:- use_module(library(random)).

/** <module> Test thread_signal/2 and related primitives.
*/

test_thread_signals :-
    run_tests([ signal_api,
		signal_catch,
		signal_nested,
		signal_exceptions
	      ]).

:- meta_predicate
    signal(:, -),
    ready(:),
    signal_self(:),
    normally(0).

:- begin_tests(signal_api, [sto(rational_trees)]).

test(pending, Pending == []) :-
    sig_pending(Pending).
test(pending, Pending == [true]) :-
    thread_self(Me),
    sig_atomic(( thread_signal(Me, true),
		 sig_pending(Pending))).
test(remove, Removed == []) :-
    sig_remove(_, Removed).
test(remove, Removed == [true]) :-
    thread_self(Me),
    sig_atomic(( thread_signal(Me, true),
		 sig_remove(X, Removed),
		 sig_pending(Pending))),
    assertion(var(X)),
    assertion(Pending == []).
test(remove, Removed == []) :-
    thread_self(Me),
    sig_atomic(( thread_signal(Me, true),
		 sig_remove(false, Removed),
		 sig_pending(Pending))),
    assertion(Pending == [true]).
test(remove, Removed == [false]) :-
    thread_self(Me),
    sig_atomic(( thread_signal(Me, false),
		 thread_signal(Me, true),
		 sig_remove(false, Removed),
		 sig_pending(Pending))),
    assertion(Pending == [true]).
test(remove, Removed == [false]) :-
    thread_self(Me),
    sig_atomic(( thread_signal(Me, true),
		 thread_signal(Me, false),
		 sig_remove(false, Removed),
		 sig_pending(Pending))),
    assertion(Pending == [true]).
test(remove, Removed == [false]) :-
    thread_self(Me),
    sig_atomic(( thread_signal(Me, true),
		 thread_signal(Me, false),
		 thread_signal(Me, ok),
		 sig_remove(false, Removed),
		 sig_pending(Pending))),
    assertion(Pending == [true,ok]).

ok.

test(backtrack, X == 42) :-
    b_setval(test_signals_v1, 0),
    thread_self(Me),
    thread_signal(Me, b_setval(test_signals_v1, 42)),
    b_getval(test_signals_v1, X).

:- end_tests(signal_api).

:- begin_tests(signal_catch, [sto(rational_trees)]).

% catch/3 processes signals during recovery
test(recover) :-
    catch(throw(error), _,
	  processes_signals).
test(atomic) :-
    sig_atomic(assertion(\+ processes_signals)).
test(cleanup) :-
    setup_call_cleanup(
	\+ processes_signals,
	processes_signals,
	\+ processes_signals).
test(signal) :-
    thread_self(Me),
    thread_signal(Me, assertion(\+ processes_signals)).

:- end_tests(signal_catch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(signal_nested, [sto(rational_trees)]).

% two nested catch calls and trigger two  signals. We test two cases. In
% the first, the inner recover is  not   atomic  and  should normally be
% interrupted by the pending signal. In nested_atomic, the inner catch/3
% recover is atomic and thus executed.

:- dynamic caught/1.

% there is no _guarantee_ the second signal arrives in time.
test(nested_likely) :-
    (   between(1, 10, _),
        test_nested_unlikely
    ->  true
    ;   \+ getenv('SWIPL_TEST_FAIL_ON_UNLIKELY', y),
        format(user_error, 'test signal_nested:nested_likely \c
			    failed (can happen)~n', [])
    ).

test_nested_unlikely :-
    retractall(caught(_)),
    signal(b-[ throw(b(1)),
	       throw(a(2))
	     ], TID),
    catch(a_non_atomic, a(X), r(X)),
    stop_signal_thread(TID),
    findall(C, retract(caught(C)), CL),
    CL == [2].

% The _=_ below and in a_atomic allow this test to work on systems without
% OS-level signals, since pending signals are only checked at the call port,
% not the exit port, and a bare true would get optimized away.
a_non_atomic :- catch((b, _=_), b(X), r(X)), sleep(1).

% here we have a guarantee as we signal ourselves inside
% sig_atomic/1.
test(nested_sure, CL==[2]) :-
    retractall(caught(_)),
    catch(a_non_atomic_2, a(X), r(X)),
    findall(C, retract(caught(C)), CL).

a_non_atomic_2 :- catch(b_2, b(X), r(X)), sleep(1).

b_2 :- signal_self([throw(b(1)),throw(a(2))]), sleep(1).

test(nested_atomic, [true(CL==[1,2]), cleanup(stop_signal_thread(TID))]) :-
    retractall(caught(_)),
    signal(b-[ throw(b(1)),
	       throw(a(2))
	     ], TID),
    catch(a_atomic, a(X), r(X)),
    findall(C, retract(caught(C)), CL).

a_atomic :- catch((b, _=_), b(X), sig_atomic(r(X))), sleep(1).

b :- ready(b), sleep(1).

r(X) :-
    assert(caught(X)).

:- end_tests(signal_nested).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(signal_exceptions, [sto(rational_trees)]).

% This test illustrates blocking  to  make   sure  the  signals are only
% processed  inside  run_guarded/0.  run_guarded/0  waits  until  it  is
% interrupted with an exception. The signal handler blocks sig/1 signals
% until run_guarded/0 re-enables them.

:- dynamic ball/1.

test(chained, Balls == Expected) :-
    Max = 10,
    numlist(1, Max, Expected),
    b_setval(test_signals_done, false),
    sig_block(sig(_)),
    ex(Max),
%   sleep(0.01),
    run,
    findall(B, retract(ball(B)), Balls0),
    msort(Balls0, Balls).	% can be out of order

run :-
    b_getval(test_signals_done, true),
    !.
run :-
    E = ball(_),
    catch(run_guarded, E, (assertz(E), run)).

run_guarded :-
    sig_unblock(sig(_)),
    loop(10 000),
    no_lco.

no_lco.

sig(done) =>
    b_setval(test_signals_done, true).
sig(N) =>
    sig_pending(Pending),
    debug(interrupt, 'Got ~p, pending: ~p', [sig(N), Pending]),
    prolog_current_frame(Frame),
    current_module(M),
    (   prolog_frame_attribute(Frame, parent_goal, M:run_guarded)
    ->  sig_block(sig(_)),
	throw(ball(N))
    ;   ansi_format(warning, 'run_guarded/0 not running~n', [])
    ).

ex(N):-
    thread_self(Me),
    thread_create(s(N, Me), _, [detached(true)]).

s(N, Me) :-
    forall(between(1, N, I),
	   ( (   maybe(0.5)
             ->  sleep(0.01)
             ;   true
             ),
	     thread_signal(Me, sig(I)))),
    thread_signal(Me, sig(done)).

loop(_) :-
    b_getval(test_signals_done, true),
    !.
loop(N) :-
    N > 0,
    !,
    N2 is N - 1,
    sleep(0.001),
    loop(N2).
loop(_).


:- end_tests(signal_exceptions).


		 /*******************************
		 *             UTILS		*
		 *******************************/

%!  processes_signals is semidet.
%
%   True if the calling thread processes signals it receives.

:- thread_local signalled/0.

processes_signals :-
    thread_self(Me),
    retractall(signalled),
    thread_signal(Me, assert(signalled)),
    loop(10),
    retract(signalled).

loop(N) :-
    N > 0,
    !,
    N2 is N - 1,
    loop(N2).
loop(_).

%!  signal(+Spec, -TID) is det.
%
%   Send some thread signals according to Spec. The core patterns of
%   spec are:
%
%     - Number-Signals
%       Delay Number seconds and send Signals
%     - Ready-Signals
%       Wait for a call to ready(Read) and send Signals.
%     - Signals
%       Send Signals.
%
%   Multiple patterns may be put in  a   list  and `Signals` is either a
%   single signal or a list of  signals.   Multiple  signals are send as
%   quickly as possible, usually  resulting  in   building  up  a signal
%   queue.

:- dynamic is_ready/2.

signal(M:List, TID) :-
    retractall(is_ready(_,_)),
    thread_self(To),
    thread_create(signal(List, To, M), TID, []).

stop_signal_thread(TID) :-
    var(TID),
    !.
stop_signal_thread(TID) :-
    catch(thread_send_message(TID, abort), error(_,_), true),
    thread_join(TID, _).

signal([], _, _) =>
    true.
signal([H|T], To, M) =>
    signal(H, To, M),
    signal(T, To, M).
signal(Delay-Signals, To, M), number(Delay) =>
    sleep(Delay),
    signal(Signals, To, M).
signal(Ready-Signals, To, M), ground(Ready) =>
    thread_wait(is_ready(M, Ready),
		[ retry_every(10),
		  wait_preds([is_ready/2])
		]),
    retractall(is_ready(M, Ready)),
    signal(Signals, To, M).
signal(Signal, To, M), callable(Signal) =>
    thread_signal(To, sig(M, Signal)).

sig(M, Signal) :-
    (   debugging(interrupt(pending))
    ->  sig_pending(Pending),
	maplist(arg(2), Pending, User),
	format('Got ~p; Pending: ~p~n', [Signal, User])
    ;   true
    ),
    (   debugging(interrupt(backtrace))
    ->  backtrace(10)
    ;   true
    ),
    call(M:Signal).

%!  ready(:Term)
%
%   Claim Term to be ready

ready(M:Term) :-
    assert(is_ready(M, Term)).

%!  signal_self(:Signals) is det.
%
%   Signal myself. Make sure the specified   signals are queued, so they
%   arrive at the same time.

signal_self(M:Signals) :-
    thread_self(To),
    sig_atomic(signal_self(Signals, To, M)).

signal_self([], _, _) =>
    true.
signal_self([H|T], To, M) =>
    signal_self(H, To, M),
    signal_self(T, To, M).
signal_self(Signal, To, M), callable(Signal) =>
    thread_signal(To, sig(M, Signal)).

%!  normally(:Goal)
%
%   Goal is normally true, but may fail in exceptional cases.

normally(Goal) :-
    getenv('SWIPL_TEST_FAIL_ON_UNLIKELY', y),
    !,
    assertion(Goal).
normally(Goal) :-
    call(Goal),
    !.
normally(Goal) :-
    print_message(warning, normally(Goal)).


:- multifile prolog:message//1.

prolog:message(normally(Goal)) -->
    [ 'Test assertion failed: ~p (ignored)'-[Goal], nl,
      'Set SWIPL_TEST_FAIL_ON_UNLIKELY=y to turn this into a test failure'-[]
    ].
