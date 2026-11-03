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


:- module(test_trace_gc,
          [ test_trace_gc/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(readutil)).

/** <module> Test the flag trace_gc

Stack GC and stack shifts report through print_message/2 at a safe
point, so the report goes to the user_error stream of the thread that
did the GC.  An exception raised while printing the report propagates.
*/

test_trace_gc :-
    run_tests([ trace_gc
              ]).

:- thread_local
    hook_action/1.

:- multifile
    user:message_hook/3.

user:message_hook(gc(done(_,_,_,_,_,_,_)), _, _) :-
    hook_action(Error),
    throw(Error).

:- begin_tests(trace_gc).

test(user_error, Status == true) :-
    thread_create(gc_to_user_error, Id),
    thread_join(Id, Status).
test(exception, Error == gc_hook_error) :-
    setup_call_cleanup(
        ( set_hook_action(gc_hook_error),
          set_prolog_flag(trace_gc, true)
        ),
        catch(( garbage_collect,
                safe_point,
                Error = none
              ),
              Error, true),
        ( set_prolog_flag(trace_gc, false),
          clear_hook_action
        )).

:- end_tests(trace_gc).

%   Outside the unit: assert/1 from the test would create a
%   hook_action/1 in the module of the unit.

set_hook_action(Error) :-
    asserta(hook_action(Error)).

clear_hook_action :-
    retractall(hook_action(_)).

%   The trace_gc report is printed when the signal is handled, i.e., at
%   the next call.

safe_point.

%!  gc_to_user_error is semidet.
%
%   Run GC with user_error bound to a file and succeed if the report
%   went there.  Must run in a thread of its own as it rebinds
%   user_error and changes the (thread-local) flag `verbose`.

gc_to_user_error :-
    tmp_file_stream(text, File, Out),
    set_stream(Out, alias(user_error)),
    set_prolog_flag(verbose, normal),       % the test suite runs silent
    set_prolog_flag(trace_gc, true),
    garbage_collect,
    safe_point,
    set_prolog_flag(trace_gc, false),
    close(Out),
    read_file_to_string(File, String, []),
    delete_file(File),
    sub_string(String, _, _, _, "GC: gained"),
    !.
