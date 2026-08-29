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

:- module(test_toplevel_color,
          [ test_toplevel_color/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(ansi_term)).

/** <module> Test colouring of the interactive toplevel

Tests the theme classes and  the  answer   counter  that  are used to
decorate the prompt, the text typed by the user and the answers.  The
actual output cannot be tested here as   that  requires a terminal. See
library(ansi_term) and the class  `answer(Parity)`   of  the  hook
prolog:console_color/2.
*/

test_toplevel_color :-
    run_tests([ toplevel_color
              ]).

:- dynamic
    user:message_property/2.

set_color_term(Old, New) :-
    current_prolog_flag(color_term, Old),
    set_prolog_flag(color_term, New).

%!  rgb_close(+RGB1, +RGB2) is semidet.
%
%   True when two colours are the same but for rounding.  The components
%   are floats in 0.0..1.0, so we can use an absolute tolerance.

rgb_close(rgb(R1,G1,B1), rgb(R2,G2,B2)) :-
    close_to(R1, R2),
    close_to(G1, G2),
    close_to(B1, B2).

close_to(V1, V2) :-
    abs(V1-V2) < 0.001.

		 /*******************************
		 *            TESTS		*
		 *******************************/

:- begin_tests(toplevel_color).

% Successive answers of one query alternate.  Redisplaying an answer
% (`w`, `p`, `+`, `-`) does not advance the counter: only
% write_bindings/4 starts a new answer.

test(stripes_alternate, Classes == [answer(odd), answer(even), answer(odd)]) :-
    '$toplevel':reset_answer_count,
    findall(C,
            ( between(1,3,_),
              '$toplevel':next_answer([x=1], true, []-[]),
              '$answer_class'(C)
            ),
            Classes).

% Only an answer that shows something is striped.  `true.` and `false.`
% are not answers to stripe, nor is the empty line that ends the
% interaction.

test(true_is_not_striped, fail) :-
    '$toplevel':reset_answer_count,
    '$toplevel':next_answer([], true, []-[]),
    '$answer_class'(_).

test(residuals_are_striped, Class = answer(_)) :-
    '$toplevel':reset_answer_count,
    '$toplevel':next_answer([], true, [freeze(_,x)]-[]),
    '$answer_class'(Class).

test(no_answer_clears, fail) :-
    '$toplevel':reset_answer_count,
    '$toplevel':next_answer([x=1], true, []-[]),
    '$toplevel':no_answer,
    '$answer_class'(_).

test(color_class, Class = answer(_)) :-
    '$toplevel':reset_answer_count,
    '$toplevel':next_answer([x=1], true, []-[]),
    '$messages':msg_color_class(query, Class).

% Without bindings the message kind is used, which has no theme entry
% and therefore no decoration.

test(color_class_undecorated, Class == query) :-
    '$toplevel':reset_answer_count,
    '$messages':msg_color_class(query, Class).

test(color_class_other, Class == error) :-
    '$messages':msg_color_class(error, Class).

% The default theme leaves the classes that need a background colour
% neutral, such that the default works on both light and dark
% terminals.

test(default_theme, true) :-
    forall(member(Class, [prompt, input, answer(odd), answer(even),
                          binding(name)]),
           assertion('$messages':default_theme(Class, _))),
    assertion('$messages':default_theme(input, [])),
    assertion('$messages':default_theme(answer(odd), [])),
    assertion('$messages':default_theme(answer(even), [])).

% ansi_sgr/2 turns a class into an escape sequence.  Note that a single
% attribute may expand to multiple codes.

test(sgr_no_color, Seq == "") :-
    setup_call_cleanup(
        set_color_term(Old, false),
        ansi_sgr(bold, Seq),
        set_prolog_flag(color_term, Old)).

test(sgr_bold, Seq == "\e[1m") :-
    setup_call_cleanup(
        set_color_term(Old, true),
        ansi_sgr(bold, Seq),
        set_prolog_flag(color_term, Old)).

test(sgr_empty_class, Seq == "") :-
    setup_call_cleanup(
        set_color_term(Old, true),
        ansi_sgr(input, Seq),          % default theme: []
        set_prolog_flag(color_term, Old)).

test(sgr_multi_code, Seq == "\e[1;48;5;235m") :-
    setup_call_cleanup(
        ( set_color_term(Old, true),
          assertz(user:message_property(test_class,
                                        color([bold, bg8(235)])))
        ),
        ansi_sgr(test_class, Seq),
        ( retractall(user:message_property(test_class, _)),
          set_prolog_flag(color_term, Old)
        )).

% A message kind without a theme entry resolves to the class term
% itself.  Decorating the message block must then silently fall back to
% plain text rather than raising a domain_error.

test(unknown_class, fail) :-
    phrase(ansi_term:sgr_codes(message(help)), _).

test(multi_code_attribute, Codes == [1,48,5,235]) :-
    phrase(ansi_term:sgr_codes([bold, bg8(235)]), Codes).

% The reply to the colour query differs between terminals: xterm uses
% four hex digits per component, the Epilog terminal two, and the
% terminator is BEL or ST.  All of these must be understood.

test(reply_two_digits, RGB == rgb(1.0,1.0,1.0)) :-
    phrase(ansi_term:color_reply(RGB), `rgb:ff/ff/ff`).

                                        % 0x8080/0xffff is 128/255
test(reply_four_digits, true(rgb_close(RGB, rgb(0.0,0.50196,1.0)))) :-
    phrase(ansi_term:color_reply(RGB), `rgb:0000/8080/ffff`).

test(reply_one_digit, RGB == rgb(1.0,0.0,0.0)) :-
    phrase(ansi_term:color_reply(RGB), `rgb:f/0/0`).

test(reply_hash, RGB == rgb(1.0,0.0,0.0)) :-
    phrase(ansi_term:color_reply(RGB), `#ff0000`).

% The number of digits the terminal uses must not change the colour we
% report: 0xf, 0xff and 0xffff are all "fully on".

test(reply_digits_agree, true) :-
    phrase(ansi_term:color_reply(RGB1), `rgb:8/8/8`),
    phrase(ansi_term:color_reply(RGB2), `rgb:88/88/88`),
    phrase(ansi_term:color_reply(RGB3), `rgb:8888/8888/8888`),
    rgb_close(RGB1, RGB2),
    rgb_close(RGB2, RGB3).

test(osc_bel, Body == `rgb:ff/ff/ff`) :-
    setup_call_cleanup(
        open_string("\e]11;rgb:ff/ff/ff\a", In),
        ansi_term:read_osc_reply(In, `11`, Body),
        close(In)).

test(osc_st, Body == `rgb:0000/0000/0000`) :-
    setup_call_cleanup(
        open_string("\e]11;rgb:0000/0000/0000\e\\", In),
        ansi_term:read_osc_reply(In, `11`, Body),
        close(In)).

% Without colour support the prompt is passed through unmodified.  This
% also covers output to a file or pipe.

test(plain_prompt, Decorated == '?- ') :-
    setup_call_cleanup(
        set_color_term(Old, false),
        '$toplevel':decorate_prompt('?- ', Decorated),
        set_prolog_flag(color_term, Old)).

:- end_tests(toplevel_color).
