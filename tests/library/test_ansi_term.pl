/*  Part of SWI-Prolog

    Author:        Robert Sedlacek
    E-mail:        rs@474.at
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(test_ansi_term,
          [ test_ansi_term/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(ansi_term)).

test_ansi_term :-
    run_tests([ansi_term]).

current_stream_tty(Stream, Tty) :-
    stream_property(Stream, tty(Tty)),
    !.
current_stream_tty(_, false).

% Capture output into a string while temporarily enabling
% terminal properties needed to trigger styled output.
captured_tty_output(Goal, Output) :-
    with_output_to(
        string(Output),
        (   current_stream_tty(current_output, PrevTty),
            current_prolog_flag(color_term, PrevColor),
            setup_call_cleanup(
                (   set_stream(current_output, tty(true)),
                    set_prolog_flag(color_term, true)
                ),
                Goal,
                (   set_stream(current_output, tty(PrevTty)),
                    set_prolog_flag(color_term, PrevColor)
                )
            )
        )
    ).

:- begin_tests(ansi_term).

% Absolute position is accurate even though escape sequences are
% injected.
test(absolute_position_output, O = "\e[mx   y\e[0m") :-
    captured_tty_output(ansi_format([], 'x~t~4|y', []), O).

% Same as `absolute_position_output` except instantiated
test(absolute_position_input) :-
    captured_tty_output(ansi_format([], 'x~t~4|y', []), "\e[mx   y\e[0m").

% Position information is correct after styled output was fully written.
% Additional outputs have correct positioning information.
test(multi_position_output, O = "\e[mx   y\e[0m\e[m   z\e[0m") :-
    captured_tty_output(
        (   ansi_format([], 'x~t~4|y', []),
            ansi_format([], '~8|z', [])
        ),
        O
    ).

% format/ansi_format argument compatibility

test(single_nonlist_argument, O = "\e[m(foo)\e[0m") :-
    captured_tty_output(ansi_format([], '(~w)', foo), O).

test(multi_arguments, O = "\e[mfoo bar\e[0m") :-
    captured_tty_output(ansi_format([], '~w~4|~w', [foo, bar]), O).

:- end_tests(ansi_term).
