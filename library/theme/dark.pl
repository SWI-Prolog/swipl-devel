/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(prolog_theme_dark, []).

/** <module> SWI-Prolog theme file -- dark

To enable the dark theme, use

    :- use_module(library(theme/dark)).
*/

:- multifile
    prolog:console_color/2.

:- if(current_predicate(win_window_color/2)).
set_window_colors :-
    win_window_color(background, rgb(0,0,0)),
    win_window_color(foreground, rgb(255,255,255)),
    win_window_color(selection_background, rgb(0,255,255)),
    win_window_color(selection_foreground, rgb(0,0,0)).

:- initialization
    set_window_colors.
:- endif.


% code embedded in messages (not used much yet)
prolog:console_color(code,                   [hfg(blue)]).
% toplevel truth value (undefined for well founded semantics)
prolog:console_color(truth(false),           [bold, fg(red)]).
prolog:console_color(truth(true),            [bold]).
prolog:console_color(truth(undefined),       [bold, fg(cyan)]).
% comment (in toplevel answers)
prolog:console_color(comment,                [hfg(green)]).
% the WFS residual program
prolog:console_color(wfs(residual_program),  [fg(cyan)]).
% trace output
prolog:console_color(frame(level),           [bold]).
prolog:console_color(port(call),             [bold, fg(green)]).
prolog:console_color(port(exit),             [bold, fg(green)]).
prolog:console_color(port(fail),             [bold, fg(red)]).
prolog:console_color(port(redo),             [bold, fg(yellow)]).
prolog:console_color(port(unify),            [bold, fg(blue)]).
prolog:console_color(port(exception),        [bold, fg(magenta)]).
% print message. the argument for debug(_) is the debug channel.
prolog:console_color(message(informational), [hfg(green)]).
prolog:console_color(message(information),   [hfg(green)]).
prolog:console_color(message(debug(_)),      [hfg(blue)]).
prolog:console_color(message(warning),       [fg(yellow)]).
prolog:console_color(message(error),         [bold, fg(red)]).

