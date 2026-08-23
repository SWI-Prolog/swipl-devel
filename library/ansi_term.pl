/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2010-2026, VU University Amsterdam
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
            ansi_format/4,              % +Stream, +Attr, +Format, +Args
            ansi_sgr/2,                 % +Attr, -Sequence
            ansi_get_color/2,           % +Which, -rgb(R,G,B)
            ansi_hyperlink/2,           % +Stream,+Location
            ansi_hyperlink/3            % +Stream,+URL,+Label
          ]).
:- autoload(library(error), [domain_error/2, must_be/2, instantiation_error/1]).
:- autoload(library(lists), [append/3, selectchk/3]).
:- autoload(library(utf8), [utf8_codes/3]).
:- autoload(library(apply), [maplist/2]).

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
    hyperlink/2,                                % +Stream, +Spec
    tty_url_hook/2.                             % +For, -URL

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

:- initialization
    init_color_term_flag.

:- multifile
    user:message_property/2.

%!  ansi_format(+ClassOrAttributes, +Format, +Args) is det.
%!  ansi_format(+Stream, +ClassOrAttributes, +Format, +Args) is det.
%
%   Format text with ANSI  attributes.   This  predicate  behaves as
%   format/2 using Format and Args, but if the `current_output` is a
%   terminal, it adds ANSI escape sequences according to Attributes.
%   For example, to print a text in bold cyan, do
%
%     ```
%     ?- ansi_format([bold,fg(cyan)], 'Hello ~w', [world]).
%     ```
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
%     - href(URL)
%       Wrap the output as a link using ansi_hyperlink/3.
%
%   Defined color constants are below. `default`   can be used to access
%   the default color of the terminal.
%
%     - black, red, green, yellow, blue, magenta, cyan, white
%
%   ANSI sequences are sent if and only if
%
%     - The `current_output` has the property tty(true) (see
%       stream_property/2).
%     - The Prolog flag `color_term` is `true`.

ansi_format(Attr, Format, Args) :-
    ansi_format(current_output, Attr, Format, Args).

ansi_format(Stream, Class, Format, Args) :-
    stream_property(Stream, tty(true)),
    current_prolog_flag(color_term, true),
    class_attrs(Class, Attr),
    Attr \== [],
    !,
    (   selectchk(href(HREF), Attr, Attr1)
    ->  true
    ;   Attr1 = Attr
    ),
    sgr_sequence(Attr1, Sequence),
    with_output_to(
        Stream,
        (   write(Sequence),
            format_content(Format, Args, HREF),
            format('\e[0m')
        )
    ),
    flush_output.
ansi_format(Stream, _Attr, Format, Args) :-
    format(Stream, Format, Args).

format_content(Format, Args, HREF) :-
    var(HREF),
    !,
    format(Format, Args).
format_content(Format, Args, HREF) :-
    format(string(Label), Format, Args),
    ansi_hyperlink(current_output, HREF, Label).

%!  ansi_sgr(+ClassOrAttributes, -Sequence:string) is det.
%
%   True when Sequence is the ANSI _Select Graphic Rendition_ sequence
%   that activates the attributes of ClassOrAttributes.  Sequence is the
%   empty string if the Prolog flag `color_term` is `false` or the class
%   resolves to no attributes.
%
%   Unlike ansi_format/4 this does not write to a stream and thus does
%   not require a terminal.  It is used to decorate strings that are
%   handed to code that is not aware of colors, notably the toplevel
%   prompt (see the Prolog flag `toplevel_prompt`).  As the caller
%   controls where the sequence ends up, the caller is also responsible
%   for verifying that the destination is a terminal and for emitting
%   the reset sequence `\e[0m`.
%
%   @see ansi_format/3 for the possible values of ClassOrAttributes.

ansi_sgr(Class, Sequence) :-
    current_prolog_flag(color_term, true),
    class_attrs(Class, Attr),
    Attr \== [],
    (   selectchk(href(_), Attr, Attr1)
    ->  true
    ;   Attr1 = Attr
    ),
    !,
    sgr_sequence(Attr1, Sequence).
ansi_sgr(_, "").

%!  sgr_sequence(+ClassOrAttributes, -Sequence:string) is det.
%
%   Sequence is the SGR escape sequence for Attributes.  Note that a
%   single attribute may map to multiple codes, e.g., bg8(Color).

sgr_sequence(Attrs, Sequence) :-
    phrase(sgr_codes_ex(Attrs), Codes),
    codes_sequence(Codes, Sequence).

codes_sequence(Codes, Sequence) :-
    atomics_to_string(Codes, ;, Code),
    format(string(Sequence), '\e[~wm', [Code]).

%!  sgr_codes(+ClassOrAttributes)// is semidet.
%
%   As sgr_codes_ex//1, but fails rather than raising an exception if
%   Attributes is not a valid attribute (list).   This is used where the
%   decoration is optional and the plain text is a fine alternative.

sgr_codes(X) -->
    { var(X),
      !,
      fail
    }.
sgr_codes([]) -->
    !.
sgr_codes([H|T]) -->
    !,
    sgr_codes(H),
    sgr_codes(T).
sgr_codes(Attr) -->
    { sgr_code(Attr, Code) },
    (   { is_list(Code) }
    ->  list(Code)
    ;   [Code]
    ).

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
%   defined in the file `boot/messages.pl`, default_theme/2.
%
%   Besides the classes used for  messages   (`code`,  `comment`, `var`,
%   `warning`, `error`, `truth(Truth)`, `port(Port)`, `message(Kind)`,
%   ...), the interactive toplevel uses these:
%
%     - prompt
%       The `?- ` prompt and its `|    ` continuation.
%     - input
%       The text typed by the user at the prompt.
%     - answer(Parity)
%       An answer written by the toplevel.  Parity is `odd` or `even`
%       and alternates over the answers of a single query, which allows
%       for _striping_ the answers using a background color.  Only an
%       answer that shows bindings, residual goals or delays uses this
%       class: ``true.`` and ``false.`` are not answers to stripe, and
%       neither is the empty line that separates the answer from the
%       next query.
%     - binding(name)
%       The variable name in a binding such as ``X = 1``.
%
%   The debugger uses, besides `frame(level)` and `port(Port)`:
%
%     - goal(Port, Parity)
%       The goal of a frame reported for Port.  Parity is `odd` or `even`
%       and alternates over the steps of a trace, which allows for
%       _striping_ the goals using a background color.  Match on Port to
%       color the goal by port instead of (or in addition to) striping.
%
%   Note that a background color on `prompt`, `input` or `answer(_)` is
%   painted up to the right margin using `\e[K`.  A background on
%   `goal(_,_)` is not: the debugger writes its ``? `` prompt on the same
%   line.
%
%   @see library(theme/dark) for an example  implementation and the Term
%   values used by the system messages.


                 /*******************************
                 *             HOOK             *
                 *******************************/

%!  prolog:message_line_element(+Stream, +Term) is semidet.
%
%   Hook implementation that colours the message elements produced by
%   print_message_lines/3.  Handled elements are:
%
%     - ansi(Class, Fmt, Args)
%     - ansi(Class, Fmt, Args, Ctx)
%       Write Fmt/Args using the attributes of Class.  As the element
%       ends with a full reset, the 4th argument version re-installs
%       the decoration of the message as a whole afterwards.
%     - url(Location)
%     - url(URL, Label)
%       Write a hyperlink.  See ansi_hyperlink/2,3.  Label is an atom or
%       string, a Format-Args pair or an ansi/3 or ansi/4 term.  The
%       latter combines a hyperlink with a style class.
%     - begin(Class, Ctx)
%     - end(Ctx)
%       Decorate the message as a whole.  See below.
%     - nl(Ctx), flush(Ctx)
%       End a line.  If the message has a background colour, the
%       remainder of the line is painted using `\e[K` (_Erase in Line_)
%       such that the coloured block extends to the right margin.
%     - eol(Ctx)
%       As above, but also reset the attributes: this ends the decorated
%       part of the line.  A message uses this for its last line if that
%       line is not ended using `nl`.  Resetting matters because a
%       terminal that scrolls while a background colour is in effect
%       paints the newly exposed line with it.
%
%   Ctx is the message _context_.  It is created by the handler for
%   begin/2 as a term
%
%       ansi(Reset, ReInstall, EraseEol)
%
%   where Reset is the sequence written by end/1, ReInstall is a
%   Format-Args pair that re-installs the attributes of the message and
%   EraseEol is the sequence that paints the remainder of the line or
%   the empty atom.  Callers must treat Ctx as opaque.  It is left
%   unbound if Stream is not a terminal, if the `color_term` flag is
%   `false` or if the message has no attributes.  All handlers that use
%   Ctx therefore fail if it is unbound, which makes
%   print_message_lines/3 fall back to writing plain text.

prolog:message_line_element(S, ansi(Class, Fmt, Args)) :-
    class_attrs(Class, Attr),
    ansi_format(S, Attr, Fmt, Args).
prolog:message_line_element(S, ansi(Class, Fmt, Args, Ctx)) :-
    class_attrs(Class, Attr),
    ansi_format(S, Attr, Fmt, Args),
    reinstall_message_attrs(S, Ctx).
prolog:message_line_element(S, nl(Ctx)) :-
    nonvar(Ctx),
    Ctx = ansi(_, _, EOL),
    write(S, EOL),
    nl(S).
prolog:message_line_element(S, flush(Ctx)) :-
    nonvar(Ctx),
    Ctx = ansi(_, _, EOL),
    write(S, EOL),
    flush_output(S).
prolog:message_line_element(S, eol(Ctx)) :-
    nonvar(Ctx),
    Ctx = ansi(Reset, _, EOL),
    write(S, EOL),
    write(S, Reset).
prolog:message_line_element(S, url(Location)) :-
    ansi_hyperlink(S, Location).
prolog:message_line_element(S, url(URL, Label)) :-
    link_label(Label, Class, Fmt, Args, Ctx),
    !,
    format(string(Text), Fmt, Args),
    (   ansi_sgr_for(S, Class, Sequence)
    ->  write(S, Sequence),
        ansi_hyperlink(S, URL, Text),
        write(S, '\e[0m'),
        reinstall_message_attrs(S, Ctx)
    ;   ansi_hyperlink(S, URL, Text)
    ).
prolog:message_line_element(S, url(URL, Label)) :-
    ansi_hyperlink(S, URL, Label).
prolog:message_line_element(S, begin(Level, Ctx)) :-
    level_attrs(Level, Attr),
    Attr \== [],
    stream_property(S, tty(true)),
    current_prolog_flag(color_term, true),
    phrase(sgr_codes(Attr), Codes),     % fails on a kind without a theme
    !,
    codes_sequence(Codes, Sequence),
    write(S, Sequence),
    erase_eol(Attr, EOL),
    Ctx = ansi('\e[0m', '\e[0m~w'-[Sequence], EOL).
prolog:message_line_element(S, end(Ctx)) :-
    nonvar(Ctx),
    Ctx = ansi(Reset, _, _),
    write(S, Reset).

%!  reinstall_message_attrs(+Stream, +Ctx) is det.
%
%   Re-install the attributes of the message as a whole after an element
%   that ended with a full reset.  See prolog:message_line_element/2.

reinstall_message_attrs(S, Ctx) :-
    (   nonvar(Ctx),
        Ctx = ansi(_, RI-RA, _)
    ->  format(S, RI, RA)
    ;   true
    ).

%!  link_label(+Label, -Class, -Format, -Args, -Ctx) is semidet.
%
%   Decompose a _decorated_ label of an  url/2 message element.  Fails if
%   Label is plain text, which is written using ansi_hyperlink/3.

link_label(Fmt-Args, -, Fmt, Args, _) :-
    atom(Fmt),
    is_list(Args),
    !.
link_label(ansi(Class, Fmt, Args), Class, Fmt, Args, _).
link_label(ansi(Class, Fmt, Args, Ctx), Class, Fmt, Args, Ctx).

%!  ansi_sgr_for(+Stream, +Class, -Sequence) is semidet.
%
%   Sequence activates the attributes of Class  on Stream.  Fails if the
%   Stream is not a terminal, if colour output is disabled or if Class
%   has no attributes.

ansi_sgr_for(S, Class, Sequence) :-
    Class \== (-),
    stream_property(S, tty(true)),
    ansi_sgr(Class, Sequence),
    Sequence \== "".

%!  erase_eol(+Attrs, -EOL) is det.
%
%   If Attrs sets a background color we must  paint the remainder of the
%   line to make the colored block  extend   to  the right margin. `\e[K`
%   (_Erase in Line_) does so on terminals   that implement _background
%   color erase_ and does not move the cursor.

erase_eol(Attrs, EOL) :-
    (   is_list(Attrs),
        has_background(Attrs)
    ->  EOL = '\e[K'
    ;   EOL = ''
    ).

has_background([H|T]) :-
    (   background(H)
    ->  true
    ;   has_background(T)
    ).

background(bg(_)).
background(bg(_,_,_)).
background(bg8(_)).
background(hbg(_)).

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
%!  ansi_hyperlink(+Stream, +Location, +Label) is det.
%
%   Create a hyperlink for  a  terminal   emulator  using  the ``OSC 8``
%   escape sequence.  Location is one of
%
%     - An absolute URL
%     - An atom (interpreted as a file name)
%     - A term `File:Line`
%     - A term `File:Line:Column`
%
%   There is no official standard for  encoding the `Line` and `Column`.
%   We emit
%
%       ``file://AbsFileName[#Line[:Column]]``
%
%   Both `Line` and `Column` count from  1,   as  in the messages we
%   print and as used by e.g., ``rg --hyperlink-format=...``.  Note that
%   a capital ``L`` before the line, as   used by GitHub, is accepted by
%   Epilog as well.
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

ansi_hyperlink(Stream, Location, Label),
    hyperlink(Stream, url(Location, Label)) =>
    true.
ansi_hyperlink(Stream, Location, Label) =>
    (   location_url(Location, URL)
    ->  format(Stream, '\e]8;;~w\e\\', [URL]),
        format(Stream, '~w', [Label]),
        format(Stream, '\e]8;;\e\\', [])
    ;   format(Stream, '~w', [Label])
    ).

%!  is_url(@URL) is semidet.
%
%   True if URL is an absolute URL. This means it has a scheme and is
%   not a (Windows) absolute file name as in `c:...`

is_url(URL) :-
    (   atom(URL)
    ->  true
    ;   string(URL)
    ),
    sub_string(URL, Before, _, _, :),
    !,
    Before > 0,
    sub_string(URL, 0, Before, _, Scheme),
    atom_codes(Scheme, Codes),
    maplist(between(0'a, 0'z), Codes),
    not_drive_scheme(Scheme).

:- if(current_prolog_flag(windows, true)).
not_drive_scheme(Scheme) :-
    \+ string_length(Scheme, 1).
:- else.
not_drive_scheme(_).
:- endif.

%!  location_url(+Location, -URL) is det.
%
%   Translate Location into a (file) URL.   This  predicate is hooked by
%   tty_url_hook/2  with  the  same  signature  to  allow  for  actions,
%   location specifiers or URL schemes.

location_url(Location, URL),
    tty_url_hook(Location, URL0) =>
    URL = URL0.
location_url(File:Line:Column, URL) =>
    url_file_name(FileURL, File),
    format(string(URL), '~w#~d:~d', [FileURL, Line, Column]).
location_url(File:Line, URL) =>
    url_file_name(FileURL, File),
    format(string(URL), '~w#~w', [FileURL, Line]).
location_url(File, URL) =>
    url_file_name(URL, File).

%!  tty_url_hook(+Location, -URL)
%
%   Hook for location_url/2.


%!  url_file_name(-URL, +File) is semidet.
%
%   Same as uri_file_name/2 in mode (-,+), but   as a core library we do
%   not wish to depend on the `clib` package and its foreign support.

url_file_name(URL, File) :-
    is_url(File), !,
    current_prolog_flag(hyperlink_term, true),
    URL = File.
url_file_name(URL, File) :-
    current_prolog_flag(hyperlink_term, true),
    absolute_file_name(File, AbsFile),
    ensure_leading_slash(AbsFile, AbsFile1),
    url_encode_path(AbsFile1, Encoded),
    format(string(URL), 'file://~s', [Encoded]).

ensure_leading_slash(Path, SlashPath) :-
    (   sub_atom(Path, 0, _, _, /)
    ->  SlashPath = Path
    ;   atom_concat(/, Path, SlashPath)
    ).

url_encode_path(Name, Encoded) :-
    atom_codes(Name, Codes),
    phrase(utf8_codes(Codes), UTF8),
    phrase(encode(UTF8), Encoded).

encode([]) --> [].
encode([H|T]) --> encode1(H), encode(T).

encode1(C) -->
    { reserved(C),
      !,
      format(codes([C1,C2]), '~`0t~16r~2|', [C])
    },
    "%", [C1,C2].
encode1(C) -->
    [C].

reserved(C) :- C =< 0'\s.
reserved(C) :- C >= 127.
reserved(0'#).

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

ansi_get_color(Which0, RGB) :-
    \+ current_prolog_flag(console_menu, true),
    stream_property(user_input, tty(true)),
    stream_property(user_output, tty(true)),
    stream_property(user_error, tty(true)),
    supports_get_color,
    (   color_alias(Which0, Which)
    ->  true
    ;   must_be(between(0,15),Which0)
    ->  Which = Which0
    ),
    catch(ansi_get_color_(Which, RGB),
          error(timeout_error(_,_), _),
          no_xterm).

supports_get_color :-
    \+ current_prolog_flag(windows, true),
    getenv('TERM', Term),
    sub_atom(Term, 0, _, _, xterm),
    \+ getenv('TERM_PROGRAM', 'Apple_Terminal').

color_alias(foreground, 10).
color_alias(background, 11).

ansi_get_color_(Which, RGB) :-
    stream_property(user_input, timeout(Old)),
    setup_call_cleanup(
        set_stream(user_input, timeout(0.05)),
        with_tty_raw(exchange_color(Which, RGB)),
        set_stream(user_input, timeout(Old))),
    !.

no_xterm :-
    print_message(warning, ansi(no_xterm_get_colour)),
    fail.

%!  exchange_color(+Which, -RGB) is semidet.
%
%   Ask the terminal for colour Which and read its reply.

exchange_color(Which, RGB) :-
    format(user_output, '\e]~w;?\a', [Which]),
    flush_output(user_output),
    format(codes(Id), '~w', [Which]),
    read_osc_reply(user_input, Id, Codes),
    phrase(color_reply(RGB), Codes).

%!  read_osc_reply(+In, +Param:codes, -Body:codes) is semidet.
%
%   Read the body of the OSC reply to  our query, i.e., the text between
%   ``ESC ] Param ;`` and the _string terminator_.  Terminals reply with
%   either BEL or ST (``ESC \``), so we accept both.
%
%   Anything that is not our reply is  echoed:  it was typed by the user
%   while we were waiting rather than sent by the terminal.

read_osc_reply(In, Param, Body) :-
    get_code(In, C),
    C \== -1,
    (   C == 0'\e,
        osc_reply(In, Param, Body0)
    ->  Body = Body0
    ;   put_code(user_output, C),
        read_osc_reply(In, Param, Body)
    ).

osc_reply(In, Param, Body) :-
    get_code(In, 0']),
    read_osc_param(In, Param),
    read_osc_string(In, Body).

read_osc_param(In, Param) :-
    read_osc_param_(In, Codes),
    Codes == Param.

read_osc_param_(In, Codes) :-
    get_code(In, C),
    C \== -1,
    (   C == 0';
    ->  Codes = []
    ;   Codes = [C|T],
        read_osc_param_(In, T)
    ).

read_osc_string(In, Codes) :-
    get_code(In, C),
    C \== -1,
    (   C == 0'\a                       % BEL
    ->  Codes = []
    ;   C == 0'\e                       % ST: ESC \
    ->  get_code(In, 0'\\),
        Codes = []
    ;   Codes = [C|T],
        read_osc_string(In, T)
    ).

%!  color_reply(-RGB)// is semidet.
%
%   Parse the body of the reply.  This is an X11 colour specification as
%   understood by XParseColor().  Terminals  differ   in  the  number of
%   digits they use per component: xterm  replies with four, others with
%   one, two or three.  A component of  N digits is scaled such that its
%   maximum maps to 0xffff, as XParseColor() does.

color_reply(rgb(R,G,B)) -->
    "rgb:", !,
    hex_component(R), "/", hex_component(G), "/", hex_component(B).
color_reply(rgb(R,G,B)) -->                     % #RGB, #RRGGBB, ...
    "#",
    hex_digits(Ds),
    { length(Ds, Len),
      N is Len//3,
      N > 0, N =< 4,
      length(RDs, N), length(GDs, N), length(BDs, N),
      append(RDs, Rest, Ds),
      append(GDs, BDs, Rest),
      hex_value(RDs, R),
      hex_value(GDs, G),
      hex_value(BDs, B)
    }.

hex_component(V) -->
    hex_digits(Ds),
    { Ds \== [],
      length(Ds, N),
      N =< 4,
      hex_value(Ds, V)
    }.

hex_digits([H|T]) -->
    [H],
    { code_type(H, xdigit(_)) },
    !,
    hex_digits(T).
hex_digits([]) -->
    [].

hex_value(Ds, V) :-
    hex_value(Ds, 0, V0),
    length(Ds, N),
    Max is 16^N - 1,
    V is V0*0xffff//Max.

hex_value([], V, V).
hex_value([D|T], V0, V) :-
    code_type(D, xdigit(DV)),
    V1 is V0*16+DV,
    hex_value(T, V1, V).

:- multifile prolog:message//1.

prolog:message(ansi(no_xterm_get_colour)) -->
    [ 'Terminal claims to be xterm compatible,'-[], nl,
      'but does not report colour info'-[]
    ].
