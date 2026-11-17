/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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

:- module(test_trace_redo_abort,
          [ test_trace_redo_abort/0
          ]).
:- use_module(library(plunit)).

/** <module> Abort at the redo port of a foreign predicate

If the debugger is at the redo port of a non-deterministic foreign
predicate such as retract/1 and the thread is aborted, the foreign
predicate was called twice with FRG_CUTTED: once by the debugger and
once when the exception discarded the frame.  For retract/1, this used
its context after it was freed.  This happened in SWISH when a user
tracing a query closed the tracer at a redo port of retract/1.
*/

test_trace_redo_abort :-
    run_tests([ trace_redo_abort
              ]).

:- begin_tests(trace_redo_abort,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(retract, Status == exception(unwind(abort))) :-
    abort_at_port(retract, redo, Status).
test(clause, Status == exception(unwind(abort))) :-
    abort_at_port(clause, redo, Status).
test(after_abort, X == 1) :-            % the predicate is usable
    abort_at_port(retract, redo, _),
    retractall(rap_fact(_)),
    assertz(rap_fact(1)),
    retract(rap_fact(X)).

:- end_tests(trace_redo_abort).

:- dynamic
    rap_fact/1.

%!  abort_at_port(+Name, +Port, -Status)
%
%   Run a goal in a thread that stops in the debugger at Port of the
%   predicate Name and abort the thread while it waits there.

abort_at_port(Name, Port, Status) :-
    thread_self(Me),
    thread_create(stop_at(Me, Name, Port), Id, []),
    (   thread_get_message(Me, stopped(Id), [timeout(10)])
    ->  thread_signal(Id, abort)
    ;   thread_signal(Id, abort)
    ),
    thread_join(Id, Status).

stop_at(Parent, Name, Port) :-
    retractall(rap_fact(_)),
    forall(between(1, 3, I), assertz(rap_fact(I))),
    nb_setval(rap_stop, stop(Parent, Name, Port)),
    trace,
    goal(Name, X),
    X > 5.

goal(retract, X) :-
    retract(rap_fact(X)).
goal(clause, X) :-
    clause(rap_fact(X), true).

:- multifile
    user:prolog_trace_interception/4.

user:prolog_trace_interception(Port, Frame, _Choice, continue) :-
    nb_current(rap_stop, stop(Parent, Name, StopPort)),
    (   port_name(Port, StopPort),
        prolog_frame_attribute(Frame, goal, Goal),
        strip_module(Goal, _, G),
        functor(G, Name, _)
    ->  nb_setval(rap_stop, done),
        thread_self(Me),
        thread_send_message(Parent, stopped(Me)),
        thread_get_message(continue)          % wait for the abort
    ;   true
    ).

port_name(redo(_), redo) :- !.
port_name(Port, Port).
