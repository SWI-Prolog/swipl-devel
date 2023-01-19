/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2023, VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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
          [ ansi_format/3,              % +Attr, +Format, +Args
            ansi_get_color/2,           % +Which, -rgb(R,G,B)
            ansi_hyperlink/2,           % +Stream,+Location
            ansi_hyperlink/3            % +Stream,+URL,+Label
          ]).
:- autoload(library(error), [domain_error/2, must_be/2, instantiation_error/1]).
:- autoload(library(lists), [append/3]).
:- autoload(library(uri), [uri_file_name/2]).
:- if(exists_source(library(time))).
:- autoload(library(time), [call_with_time_limit/2]).
:- endif.


/** <module> Print decorated text to ANSI consoles

This library allows for exploiting the color and attribute facilities of
most modern terminals using ANSI escape sequences. This library provides
the following:

  - ansi_format/3 allows writing messages to the terminal with ansi
    attributes.
  - It defines the hook prolog:message_line_element/2, which provides
    ansi attributes and hyperlinks for print_message/2.

The behavior of this library is controlled by two Prolog flags:

  - `color_term`
    When `true`, activate the color output for this library.  Otherwise
    simply call format/3.
  - `hyperlink_term`
    Emit terminal hyperlinks for url(Location) and url(URL, Label)
    elements of Prolog messages.

@see    http://en.wikipedia.org/wiki/ANSI_escape_code
*/

:- multifile
    prolog:console_color/2,                     % +Term, -AnsiAttrs
    supports_get_color/0,
    hyperlink/2.                                % +Stream, +Spec


color_term_flag_default(true) :-
    stream_property(user_input, tty(true)),
    stream_property(user_error, tty(true)),
    stream_property(user_output, tty(true)),
    \+ getenv('TERM', dumb),
    !.
color_term_flag_default(false).

init_color_term_flag :-
    color_term_flag_default(Default),
    create_prolog_flag(color_term, Default,
                       [ type(boolean),
                         keep(true)
                       ]),
    create_prolog_flag(hyperlink_term, false,
                       [ type(boolean),
                         keep(true)
                       ]).

:- init_color_term_flag.


:- meta_predicate
    keep_line_pos(+, 0).

:- multifile
    user:message_property/2.

%!  ansi_format(+ClassOrAttributes, +Format, +Args) is det.
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
%   Attributes is either a single attribute, a   list  thereof or a term
%   that is mapped to concrete  attributes   based  on the current theme
%   (see prolog:console_color/2). The attribute names   are derived from
%   the ANSI specification. See the source   for sgr_code/2 for details.
%   Some commonly used attributes are:
%
%     - bold
%     - underline
%     - fg(Color), bg(Color), hfg(Color), hbg(Color)
%       For fg(Color) and bg(Color), the colour name can be '#RGB' or
%       '#RRGGBB'
%     - fg8(Spec), bg8(Spec)
%       8-bit color specification.  Spec is a colour name, h(Color)
%       or an integer 0..255.
%     - fg(R,G,B), bg(R,G,B)
%       24-bit (direct color) specification.  The components are
%       integers in the range 0..255.
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

ansi_format(Stream, Class, Format, Args) :-
    stream_property(Stream, tty(true)),
    current_prolog_flag(color_term, true),
    !,
    class_attrs(Class, Attr),
    phrase(sgr_codes_ex(Attr), Codes),
    atomic_list_concat(Codes, ;, Code),
    with_output_to(
        Stream,
        (   keep_line_pos(current_output, format('\e[~wm', [Code])),
            format(Format, Args),
            keep_line_pos(current_output, format('\e[0m'))
        )
    ),
    flush_output.
ansi_format(Stream, _Attr, Format, Args) :-
    format(Stream, Format, Args).

sgr_codes_ex(X) -->
    { var(X),
      !,
      instantiation_error(X)
    }.
sgr_codes_ex([]) -->
    !.
sgr_codes_ex([H|T]) -->
    !,
    sgr_codes_ex(H),
    sgr_codes_ex(T).
sgr_codes_ex(Attr) -->
    (   { sgr_code(Attr, Code) }
    ->  (   { is_list(Code) }
        ->  list(Code)
        ;   [Code]
        )
    ;   { domain_error(sgr_code, Attr) }
    ).

list([]) --> [].
list([H|T]) --> [H], list(T).


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
    (   ansi_color(Name, N)
    ->  C is N+30
    ;   rgb(Name, R, G, B)
    ->  sgr_code(fg(R,G,B), C)
    ).
sgr_code(bg(Name), C) :-
    !,
    (   ansi_color(Name, N)
    ->  C is N+40
    ;   rgb(Name, R, G, B)
    ->  sgr_code(bg(R,G,B), C)
    ).
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
sgr_code(fg8(Name), [38,5,N]) :-
    ansi_color8(Name, N).
sgr_code(bg8(Name), [48,5,N]) :-
    ansi_color8(Name, N).
sgr_code(fg(R,G,B), [38,2,R,G,B]) :-
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B).
sgr_code(bg(R,G,B), [48,2,R,G,B]) :-
    between(0, 255, R),
    between(0, 255, G),
    between(0, 255, B).

off_code(italic_and_franktur, 23).
off_code(underline, 24).
off_code(blink, 25).
off_code(negative, 27).
off_code(conceal, 28).
off_code(crossed_out, 29).
off_code(framed, 54).
off_code(overlined, 55).

ansi_color8(h(Name), N) :-
    !,
    ansi_color(Name, N0),
    N is N0+8.
ansi_color8(Name, N) :-
    atom(Name),
    !,
    ansi_color(Name, N).
ansi_color8(N, N) :-
    between(0, 255, N).

ansi_color(black,   0).
ansi_color(red,     1).
ansi_color(green,   2).
ansi_color(yellow,  3).
ansi_color(blue,    4).
ansi_color(magenta, 5).
ansi_color(cyan,    6).
ansi_color(white,   7).
ansi_color(default, 9).

rgb(Name, R, G, B) :-
    atom_codes(Name, [0'#,R1,R2,G1,G2,B1,B2]),
    hex_color(R1,R2,R),
    hex_color(G1,G2,G),
    hex_color(B1,B2,B).
rgb(Name, R, G, B) :-
    atom_codes(Name, [0'#,R1,G1,B1]),
    hex_color(R1,R),
    hex_color(G1,G),
    hex_color(B1,B).

hex_color(D1,D2,V) :-
    code_type(D1, xdigit(V1)),
    code_type(D2, xdigit(V2)),
    V is 16*V1+V2.

hex_color(D1,V) :-
    code_type(D1, xdigit(V1)),
    V is 16*V1+V1.

%!  prolog:console_color(+Term, -AnsiAttributes) is semidet.
%
%   Hook that allows  for  mapping  abstract   terms  to  concrete  ANSI
%   attributes. This hook  is  used  by   _theme_  files  to  adjust the
%   rendering based on  user  preferences   and  context.  Defaults  are
%   defined in the file `boot/messages.pl`.
%
%   @see library(theme/dark) for an example  implementation and the Term
%   values used by the system messages.


                 /*******************************
                 *             HOOK             *
                 *******************************/

%!  prolog:message_line_element(+Stream, +Term) is semidet.
%
%   Hook implementation that deals with  ansi(+Attr, +Fmt, +Args) in
%   message specifications.

prolog:message_line_element(S, ansi(Class, Fmt, Args)) :-
    class_attrs(Class, Attr),
    ansi_format(S, Attr, Fmt, Args).
prolog:message_line_element(S, ansi(Class, Fmt, Args, Ctx)) :-
    class_attrs(Class, Attr),
    ansi_format(S, Attr, Fmt, Args),
    (   nonvar(Ctx),
        Ctx = ansi(_, RI-RA)
    ->  keep_line_pos(S, format(S, RI, RA))
    ;   true
    ).
prolog:message_line_element(S, url(Location)) :-
    ansi_hyperlink(S, Location).
prolog:message_line_element(S, url(URL, Label)) :-
    ansi_hyperlink(S, URL, Label).
prolog:message_line_element(S, begin(Level, Ctx)) :-
    level_attrs(Level, Attr),
    stream_property(S, tty(true)),
    current_prolog_flag(color_term, true),
    !,
    (   is_list(Attr)
    ->  sgr_codes(Attr, Codes),
        atomic_list_concat(Codes, ;, Code)
    ;   sgr_code(Attr, Code)
    ),
    keep_line_pos(S, format(S, '\e[~wm', [Code])),
    Ctx = ansi('\e[0m', '\e[0m\e[~wm'-[Code]).
prolog:message_line_element(S, end(Ctx)) :-
    nonvar(Ctx),
    Ctx = ansi(Reset, _),
    keep_line_pos(S, write(S, Reset)).

sgr_codes([], []).
sgr_codes([H0|T0], [H|T]) :-
    sgr_code(H0, H),
    sgr_codes(T0, T).

level_attrs(Level,         Attrs) :-
    user:message_property(Level, color(Attrs)),
    !.
level_attrs(Level,         Attrs) :-
    class_attrs(message(Level), Attrs).

class_attrs(Class, Attrs) :-
    user:message_property(Class, color(Attrs)),
    !.
class_attrs(Class, Attrs) :-
    prolog:console_color(Class, Attrs),
    !.
class_attrs(Class, Attrs) :-
    '$messages':default_theme(Class, Attrs),
    !.
class_attrs(Attrs, Attrs).

%!  ansi_hyperlink(+Stream, +Location) is det.
%!  ansi_hyperlink(+Stream, +URL, +Label) is det.
%
%   Create a hyperlink for a terminal emulator. The file is fairly easy,
%   but getting the line and column across is   not as there seems to be
%   no established standard. The  current   implementation  emits, i.e.,
%   inserting a capital ``L`` before the line.
%
%       ``file://AbsFileName[#LLine[:Column]]``
%
%   @see https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda

ansi_hyperlink(Stream, Location) :-
    hyperlink(Stream, url(Location)),
    !.
ansi_hyperlink(Stream, Location) :-
    location_label(Location, Label),
    ansi_hyperlink(Stream, Location, Label).

location_label(File:Line:Column, Label) =>
    format(string(Label), '~w:~w:~w', [File,Line,Column]).
location_label(File:Line, Label) =>
    format(string(Label), '~w:~w', [File,Line]).
location_label(File, Label) =>
    format(string(Label), '~w', [File]).

ansi_hyperlink(Stream, Location, Label) :-
    hyperlink(Stream, url(Location, Label)),
    !.
ansi_hyperlink(Stream, File:Line:Column, Label) :-
    !,
    (   url_file_name(URI, File)
    ->  format(Stream, '\e]8;;~w#~d:~d\e\\~w\e]8;;\e\\',
               [ URI, Line, Column, Label ])
    ;   format(Stream, '~w', [Label])
    ).
ansi_hyperlink(Stream, File:Line, Label) :-
    !,
    (   url_file_name(URI, File)
    ->  format(Stream, '\e]8;;~w#~w\e\\~w\e]8;;\e\\',
               [ URI, Line, Label ])
    ;   format(Stream, '~w:~w', [File, Line])
    ).
ansi_hyperlink(Stream, File, Label) :-
    (   url_file_name(URI, File)
    ->  format(Stream, '\e]8;;~w\e\\~w\e]8;;\e\\',
               [ URI, Label ])
    ;   format(Stream, '~w', [File])
    ).



%!  hyperlink(+Stream, +Spec) is semidet.
%
%   Multifile hook that may be used   to redefine ansi_hyperlink/2,3. If
%   this predicate succeeds the system assumes the link has been written
%   to Stream.
%
%   @arg  Spec  is  either  url(Location)    or   url(URL,  Label).  See
%   ansi_hyperlink/2,3 for details.

:- dynamic has_lib_uri/1 as volatile.

url_file_name(URL, File) :-
    current_prolog_flag(hyperlink_term, true),
    (   has_lib_uri(true)
    ->  uri_file_name(URL, File)
    ;   exists_source(library(uri))
    ->  use_module(library(uri), [uri_file_name/2]),
        uri_file_name(URL, File),
        asserta(has_lib_uri(true))
    ;   asserta(has_lib_uri(false)),
        fail
    ).

%!  keep_line_pos(+Stream, :Goal)
%
%   Run goal without changing the position   information on Stream. This
%   is used to avoid that the exchange   of  ANSI sequences modifies the
%   notion of, notably, the `line_pos` notion.

keep_line_pos(S, G) :-
    stream_property(S, position(Pos)),
    !,
    setup_call_cleanup(
        stream_position_data(line_position, Pos, LPos),
        G,
        set_stream(S, line_position(LPos))).
keep_line_pos(_, G) :-
    call(G).

%!  ansi_get_color(+Which, -RGB) is semidet.
%
%   Obtain the RGB color for an ANSI  color parameter. Which is either a
%   color alias or  an  integer  ANSI   color  id.  Defined  aliases are
%   `foreground` and `background`. This predicate sends a request to the
%   console (`user_output`) and reads the reply. This assumes an `xterm`
%   compatible terminal.
%
%   @arg RGB is a term rgb(Red,Green,Blue).  The color components are
%   integers in the range 0..65535.


:- if(current_predicate(call_with_time_limit/2)).
ansi_get_color(Which0, RGB) :-
    stream_property(user_input, tty(true)),
    stream_property(user_output, tty(true)),
    stream_property(user_error, tty(true)),
    supports_get_color,
    (   color_alias(Which0, Which)
    ->  true
    ;   must_be(between(0,15),Which0)
    ->  Which = Which0
    ),
    catch(keep_line_pos(user_output,
                        ansi_get_color_(Which, RGB)),
          time_limit_exceeded,
          no_xterm).

supports_get_color :-
    getenv('TERM', Term),
    sub_atom(Term, 0, _, _, xterm),
    \+ getenv('TERM_PROGRAM', 'Apple_Terminal').

color_alias(foreground, 10).
color_alias(background, 11).

ansi_get_color_(Which, rgb(R,G,B)) :-
    format(codes(Id), '~w', [Which]),
    hex4(RH),
    hex4(GH),
    hex4(BH),
    phrase(("\e]", Id, ";rgb:", RH, "/", GH, "/", BH, "\a"), Pattern),
    call_with_time_limit(0.05,
                         with_tty_raw(exchange_pattern(Which, Pattern))),
    !,
    hex_val(RH, R),
    hex_val(GH, G),
    hex_val(BH, B).

no_xterm :-
    print_message(warning, ansi(no_xterm_get_colour)),
    fail.

hex4([_,_,_,_]).

hex_val([D1,D2,D3,D4], V) :-
    code_type(D1, xdigit(V1)),
    code_type(D2, xdigit(V2)),
    code_type(D3, xdigit(V3)),
    code_type(D4, xdigit(V4)),
    V is (V1<<12)+(V2<<8)+(V3<<4)+V4.

exchange_pattern(Which, Pattern) :-
    format(user_output, '\e]~w;?\a', [Which]),
    flush_output(user_output),
    read_pattern(user_input, Pattern, []).

read_pattern(From, Pattern, NotMatched0) :-
    copy_term(Pattern, TryPattern),
    append(Skip, Rest, NotMatched0),
    append(Rest, RestPattern, TryPattern),
    !,
    echo(Skip),
    try_read_pattern(From, RestPattern, NotMatched, Done),
    (   Done == true
    ->  Pattern = TryPattern
    ;   read_pattern(From, Pattern, NotMatched)
    ).

%!  try_read_pattern(+From, +Pattern, -NotMatched)

try_read_pattern(_, [], [], true) :-
    !.
try_read_pattern(From, [H|T], [C|RT], Done) :-
    get_code(C),
    (   C = H
    ->  try_read_pattern(From, T, RT, Done)
    ;   RT = [],
        Done = false
    ).

echo([]).
echo([H|T]) :-
    put_code(user_output, H),
    echo(T).

:- else.
ansi_get_color(_Which0, _RGB) :-
    fail.
:- endif.



:- multifile prolog:message//1.

prolog:message(ansi(no_xterm_get_colour)) -->
    [ 'Terminal claims to be xterm compatible,'-[], nl,
      'but does not report colour info'-[]
    ].
