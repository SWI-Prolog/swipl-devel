/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(test_signal_gc,
          [ test_signal_gc/0
          ]).
:- use_module(library(plunit)).

/** <module> Signal handlers that GC inside built-ins

Built-ins that call PL_handle_signals() while walking a term must not
keep raw pointers across this call: the handler may run Prolog code,
which may garbage collect or shift the stacks.  The tests interrupt a
long walk with an alarm whose handler moves the data.  A missed
interrupt makes a test pass spuriously, never fail.
*/

test_signal_gc :-
    run_tests([ signal_gc
              ]).

:- if(exists_source(library(time))).
:- use_module(library(time)).

:- begin_tests(signal_gc).

test(seek_list, [condition(have_alarms), Failed == []]) :-
    numlist(0, 3 000 000, L),
    findall(I, ( between(1, 10, I),
                 \+ seek_with_alarm(L)
               ), Failed).

:- end_tests(signal_gc).

have_alarms :-
    catch(( alarm(60, true, Id, [remove(true)]),
            remove_alarm(Id)
          ), _, fail).

seek_with_alarm(L) :-
    setup_call_cleanup(
        alarm(0.0003, move_data, Id, [remove(true)]),
        nth0(2 999 999, L, E),          % uses '$seek_list'/4
        catch(remove_alarm(Id), _, true)),
    E == 2 999 999.

move_data :-
    trim_stacks,
    numlist(1, 2 000 000, G),
    length(G, _),
    garbage_collect.

:- else.

test_signal_gc.

:- endif.
