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


:- module(test_profile,
          [ test_profile/0
          ]).
:- use_module(library(plunit)).
:- if(exists_source(library(prolog_profile))).
:- use_module(library(prolog_profile)).
:- use_module(library(lists)).

/** <module> Test the execution profiler

The exact modes read the clock at each change  of the current node in
the call tree.  We use sleep/1 to get  bounds on the recorded times that
hold regardless of the machine load:  sleeping   takes  wall time but no
CPU time.
*/

test_profile :-
    run_tests([profile_report, profile_default, profile_exact]).

:- begin_tests(profile_report).

% Only one thread can be profiled.  profile/2 in another thread must
% raise an error and leave the profiler of the first thread alone.

test(busy, [ true(Error = error(permission_error(profile, thread, _), _)),
             true(Mode == cputime)
           ]) :-
    thread_self(Me),
    thread_create(( profiler(_, cputime),
                    thread_send_message(Me, profiling),
                    thread_get_message(done),
                    profiler(Mode0, false),
                    thread_send_message(Me, mode(Mode0))
                  ), Id, [detached(true)]),
    thread_get_message(profiling),
    catch(profile(true), Error, true),
    thread_send_message(Id, done),
    thread_get_message(mode(Mode)).

% A sampled profile that is too short for a sample has no ticks.

test(no_samples, [ true(Ticks == 0),
                   true(sub_string(Report, _, _, _, "Total time"))
                 ]) :-
    reset_profiler,
    profiler(_, walltime),
    profiler(_, false),
    profile_data(Data),
    Ticks = Data.summary.ticks,
    with_output_to(string(Report),
                   prolog_profile:show_text_profile([top(1)])).

% A label that is too wide for its column is elided, so the columns of
% its row stay aligned.

test(long_label, Width == 102) :-
    reset_profiler,
    setup_call_cleanup(
        profiler(_, cputime),
        a_predicate_with_a_name_that_is_far_too_long_for_its_column,
        profiler(_, false)),
    with_output_to(string(Report),
                   prolog_profile:show_text_profile([top(100)])),
    split_string(Report, "\n", "", Rows),
    member(Row, Rows),
    sub_string(Row, _, _, _, "_for_its_column/0"),
    !,
    string_length(Row, Width).

% profiler/2 is still running when the profile is collected: it called,
% but did not exit.  That used to make its redo count -1, i.e., 2^64-1.
% '$profile'/4 sets the ports that profiler/2 counts.

test(running_call_has_no_redos, Redo == 0) :-
    '$profile'(true, cputime, true, 200),
    reset_profiler,
    profiler(_, cputime),
    profiler(_, false),
    profile_data(Data),
    member(Node, Data.nodes),
    Node.predicate == system:profiler/2,
    !,
    Redo = Node.redo.

% Counts that are wider than their column move the columns, so they stay
% aligned.  The rows of the report have their "+" signs in the same
% columns.

test(wide_counts, Columns == [Plus]) :-
    reset_profiler,
    '$profile'(many_redos, cputime, true, 200),
    with_output_to(string(Report),
                   prolog_profile:show_text_profile([top(100)])),
    split_string(Report, "\n", "", Rows),
    findall(Ps, ( member(Row, Rows),
                  sub_string(Row, _, _, _, "s.("),
                  findall(P, sub_string(Row, P, _, _, "+"), Ps)
                ), Columns0),
    sort(Columns0, Columns),
    Columns = [Plus].

:- end_tests(profile_report).

a_predicate_with_a_name_that_is_far_too_long_for_its_column.

many_redos :-
    \+ ( between(1, 2 000 000, X), X < 0 ).

:- begin_tests(profile_default, [condition(text_report)]).

% With profile_exact = auto, profile/2 uses exact timing if the clock is
% cheap to read, which is typically not the case in virtual machines.

test(auto, Exact == Expected) :-
    (   '$prof_exact_ok'(cpu)
    ->  Expected = true
    ;   Expected = false
    ),
    profile_with_flag(auto, Exact).
test(flag_false, Exact == false) :-
    profile_with_flag(false, Exact).

:- end_tests(profile_default).

%   The report of profile/2 is textual unless the GUI hooks into it.

text_report :-
    \+ ( predicate_property(prolog:show_profile_hook(_),
                            number_of_clauses(N)),
         N > 0
       ).

profile_with_flag(Value, Exact) :-
    current_prolog_flag(profile_exact, Old),
    setup_call_cleanup(
        set_prolog_flag(profile_exact, Value),
        with_output_to(string(_), profile(true, [top(1)])),
        set_prolog_flag(profile_exact, Old)),
    profile_data(Data),
    Exact = Data.summary.exact.

:- begin_tests(profile_exact).

test(mode, Mode == exact_walltime) :-
    reset_profiler,
    profiler(_, exact_walltime),
    profiler(Mode, false).
test(sleep, [ true(Ticks >= 20 000 000),
              true(Self >= 20 000 000),
              true(Ticks >= Self)
            ]) :-
    exact_profile(sleep_twice, Data, exact_walltime),
    Ticks = Data.summary.ticks,
    self_ticks(Data, system:sleep/1, Self).
test(cpu_mode, [ condition(exact_cpu), Mode == exact_cputime ]) :-
    reset_profiler,
    profiler(_, exact_cputime),
    profiler(Mode, false).
test(cpu_sleep, [ condition(exact_cpu),
                  true(Self*2 < Busy)
                ]) :-
    exact_profile(sleep_and_work, Data, exact_cputime),
    self_ticks(Data, system:sleep/1, Self),
    self_ticks(Data, lists:numlist_/3, Busy).

:- end_tests(profile_exact).

sleep_twice :-
    sleep(0.01),
    sleep(0.01).

% exact_cputime requires a precise thread CPU clock, which is
% not available on, e.g., Wine.

exact_cpu :-
    reset_profiler,
    catch(profiler(_, exact_cputime), error(not_implemented(_,_),_), fail),
    profiler(_, false).

% Sleep in short intervals, such that  sleeping and working alternate
% within the windows in which the CPU time is read.

sleep_and_work :-
    forall(between(1, 20, _),
           ( sleep(0.001),
             numlist(1, 20 000, _)
           )).

self_ticks(Data, PI, Ticks) :-
    member(Node, Data.nodes),
    Node.predicate == PI,
    !,
    Ticks = Node.ticks_self.

exact_profile(Goal, Data, Mode) :-
    reset_profiler,
    setup_call_cleanup(
        profiler(_, Mode),
        Goal,
        profiler(_, false)),
    profile_data(Data).

:- else.                                % no profiler in this build

test_profile.

:- endif.
