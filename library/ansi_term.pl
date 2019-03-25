/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, VU University Amsterdam
                              CWI, Amsterdam
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

:- module(ansi_term,
          [ ansi_format/3               % +Attr, +Format, +Args
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).

/** <module> Print decorated text to ANSI consoles

This library allows for exploiting the color and attribute facilities of
most modern terminals using ANSI escape sequences. This library provides
the following:

  - ansi_format/3 allows writing messages to the terminal with ansi
    attributes.
  - It defines the hook prolog:message_line_element/2, which provides
    ansi attributes for print_message/2.

@see    http://en.wikipedia.org/wiki/ANSI_escape_code
*/

:- create_prolog_flag(color_term, true,
                      [ type(boolean),
                        keep(true)
                      ]).

:- meta_predicate
    keep_line_pos(+, 0).

:- multifile
    user:message_property/2.

%!  ansi_format(+Attributes, +Format, +Args) is det.
%
%   Format text with ANSI  attributes.   This  predicate  behaves as
%   format/2 using Format and Args, but if the =current_output= is a
%   terminal, it adds ANSI escape sequences according to Attributes.
%   For example, to print a text in bold cyan, do
%
%     ==
%     ?- ansi_format([bold,fg(cyan)], 'Hello ~w', [world]).
%     ==
%
%   Attributes is either a single attribute   or a list thereof. The
%   attribute names are derived from the ANSI specification. See the
%   source for sgr_code/2 for details. Some commonly used attributes
%   are:
%
%     - bold
%     - underline
%     - fg(Color), bg(Color), hfg(Color), hbg(Color)
%
%   Defined color constants are below.  =default=   can  be  used to
%   access the default color of the terminal.
%
%     - black, red, green, yellow, blue, magenta, cyan, white
%
%   ANSI sequences are sent if and only if
%
%     - The =current_output= has the property tty(true) (see
%       stream_property/2).
%     - The Prolog flag =color_term= is =true=.

ansi_format(Attr, Format, Args) :-
    ansi_format(current_output, Attr, Format, Args).

ansi_format(Stream, Attr, Format, Args) :-
    stream_property(Stream, tty(true)),
    current_prolog_flag(color_term, true),
    !,
    (   is_list(Attr)
    ->  maplist(sgr_code_ex, Attr, Codes),
        atomic_list_concat(Codes, ;, Code)
    ;   sgr_code_ex(Attr, Code)
    ),
    format(string(Fmt), '\e[~~wm~w\e[0m', [Format]),
    format(Stream, Fmt, [Code|Args]),
    flush_output.
ansi_format(Stream, _Attr, Format, Args) :-
    format(Stream, Format, Args).

sgr_code_ex(Attr, Code) :-
    sgr_code(Attr, Code),
    !.
sgr_code_ex(Attr, _) :-
    domain_error(sgr_code, Attr).

%!  sgr_code(+Name, -Code)
%
%   True when code is the Select   Graphic  Rendition code for Name.
%   The defined names are given below. Note that most terminals only
%   implement this partially.
%
%     | reset                       | all attributes off    |
%     | bold                        |                       |
%     | faint                       |       |
%     | italic                      |       |
%     | underline                   |       |
%     | blink(slow)                 |       |
%     | blink(rapid)                |       |
%     | negative                    |       |
%     | conceal                     |       |
%     | crossed_out                 |       |
%     | font(primary)               |       |
%     | font(N)                     | Alternate font (1..8) |
%     | fraktur                     |       |
%     | underline(double)           |       |
%     | intensity(normal)           |       |
%     | fg(Name)                    | Color name    |
%     | bg(Name)                    | Color name    |
%     | framed                      |       |
%     | encircled                   |       |
%     | overlined                   |       |
%     | ideogram(underline)         |       |
%     | right_side_line             |       |
%     | ideogram(underline(double)) |       |
%     | right_side_line(double)     |       |
%     | ideogram(overlined)         |       |
%     | left_side_line              |       |
%     | ideogram(stress_marking)    |       |
%     | -Off                        | Switch attributes off |
%     | hfg(Name)                   | Color name    |
%     | hbg(Name)                   | Color name    |
%
%   @see http://en.wikipedia.org/wiki/ANSI_escape_code

sgr_code(reset, 0).
sgr_code(bold,  1).
sgr_code(faint, 2).
sgr_code(italic, 3).
sgr_code(underline, 4).
sgr_code(blink(slow), 5).
sgr_code(blink(rapid), 6).
sgr_code(negative, 7).
sgr_code(conceal, 8).
sgr_code(crossed_out, 9).
sgr_code(font(primary), 10) :- !.
sgr_code(font(N), C) :-
    C is 10+N.
sgr_code(fraktur, 20).
sgr_code(underline(double), 21).
sgr_code(intensity(normal), 22).
sgr_code(fg(Name), C) :-
    ansi_color(Name, N),
    C is N+30.
sgr_code(bg(Name), C) :-
    !,
    ansi_color(Name, N),
    C is N+40.
sgr_code(framed, 51).
sgr_code(encircled, 52).
sgr_code(overlined, 53).
sgr_code(ideogram(underline), 60).
sgr_code(right_side_line, 60).
sgr_code(ideogram(underline(double)), 61).
sgr_code(right_side_line(double), 61).
sgr_code(ideogram(overlined), 62).
sgr_code(left_side_line, 62).
sgr_code(ideogram(stress_marking), 64).
sgr_code(-X, Code) :-
    off_code(X, Code).
sgr_code(hfg(Name), C) :-
    ansi_color(Name, N),
    C is N+90.
sgr_code(hbg(Name), C) :-
    !,
    ansi_color(Name, N),
    C is N+100.

off_code(italic_and_franktur, 23).
off_code(underline, 24).
off_code(blink, 25).
off_code(negative, 27).
off_code(conceal, 28).
off_code(crossed_out, 29).
off_code(framed, 54).
off_code(overlined, 55).


ansi_color(black,   0).
ansi_color(red,     1).
ansi_color(green,   2).
ansi_color(yellow,  3).
ansi_color(blue,    4).
ansi_color(magenta, 5).
ansi_color(cyan,    6).
ansi_color(white,   7).
ansi_color(default, 9).


                 /*******************************
                 *             HOOK             *
                 *******************************/

%!  prolog:message_line_element(+Stream, +Term) is semidet.
%
%   Hook implementation that deals with  ansi(+Attr, +Fmt, +Args) in
%   message specifications.

prolog:message_line_element(S, ansi(Attr, Fmt, Args)) :-
    ansi_format(S, Attr, Fmt, Args).
prolog:message_line_element(S, ansi(Attr, Fmt, Args, Ctx)) :-
    ansi_format(S, Attr, Fmt, Args),
    (   nonvar(Ctx),
        Ctx = ansi(_, RI-RA)
    ->  keep_line_pos(S, format(S, RI, RA))
    ;   true
    ).
prolog:message_line_element(S, begin(Level, Ctx)) :-
    level_attrs(Level, Attr),
    stream_property(S, tty(true)),
    current_prolog_flag(color_term, true),
    !,
    (   is_list(Attr)
    ->  maplist(sgr_code, Attr, Codes),
        atomic_list_concat(Codes, ;, Code)
    ;   sgr_code(Attr, Code)
    ),
    keep_line_pos(S, format(S, '\e[~wm', [Code])),
    Ctx = ansi('\e[0m', '\e[0m\e[~wm'-[Code]).
prolog:message_line_element(S, end(Ctx)) :-
    nonvar(Ctx),
    Ctx = ansi(Reset, _),
    keep_line_pos(S, write(S, Reset)).

level_attrs(Level,         Attrs) :-
    user:message_property(Level, color(Attrs)).
level_attrs(informational, fg(green)).
level_attrs(information,   fg(green)).
level_attrs(debug(_),      fg(blue)).
level_attrs(warning,       fg(red)).
level_attrs(error,         [fg(red),bold]).

keep_line_pos(S, G) :-
    stream_property(S, position(Pos)),
    !,
    stream_position_data(line_position, Pos, LPos),
    G,
    set_stream(S, line_position(LPos)).
keep_line_pos(_, G) :-
    G.
