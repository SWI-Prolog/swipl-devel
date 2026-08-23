/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(prolog_theme_light, []).

/** <module> SWI-Prolog theme file -- light

The `light` theme is the default. It  only   defines  the parts that the
default theme (see default_theme/2 in `boot/messages.pl`) leaves neutral
because they require knowledge about  the   background  colour  of the
terminal.
*/

:- multifile
    prolog:theme/1,
    prolog:console_color/2.

prolog:theme(light).                             % make ourselves known

		 /*******************************
		 *       PROLOG MESSAGES	*
		 *******************************/

% interactive toplevel.  The command line (prompt and the text typed by
% the user) has its own background.  The answers to a single query
% alternate between two backgrounds, which separates the answers of a
% non-deterministic query.
prolog:console_color(prompt,        [bold, fg8(blue), bg8(123)]).
prolog:console_color(input,         [bg8(123)]).
prolog:console_color(answer(odd),   [bg8(255)]).
prolog:console_color(answer(even),  [bg8(253)]).
prolog:console_color(binding(name), [bold, fg8(magenta)]).
% trace output.  The goal of successive steps alternates between two
% backgrounds, which separates the steps of a trace.  The first argument
% is the port, which allows for colouring the goal by port instead of
% (or in addition to) striping.
prolog:console_color(goal(_, odd),  [fg(blue), bg8(254)]).
prolog:console_color(goal(_, even), [fg(blue), bg8(252)]).
% tag that indicates the kind of a predicate in a list of candidates.
% Not blue: that is the colour of the `code` class used for the
% predicate indicator the tag belongs to.
prolog:console_color(predicate(iso),        [fg8(magenta)]).
prolog:console_color(predicate(built_in),   [fg8(magenta)]).
prolog:console_color(predicate(foreign),    [fg8(magenta)]).
prolog:console_color(predicate(library(_)), [fg8(green)]).
prolog:console_color(predicate(module(_)),  [fg8(green)]).
prolog:console_color(predicate(user),       [fg(default)]).
prolog:console_color(predicate(undefined),  [fg(red)]).
prolog:console_color(message(Level), Attrs) :-
    nonvar(Level),
    prolog:console_color(Level, Attrs).
