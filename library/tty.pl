/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(tty,
        [ tty_clear/0
        , tty_flash/0
        , menu/3
        ]).
:- use_module(library(lists), [append/3, nth1/3]).

/** <module> Terminal operations

This library package defines some common operations on terminals. It is
based on the Unix termcap facility to perform terminal independant I/O
on video displays. The package consists of three sections:

  1. Predicates to perform simple operations on terminals
  2. Extenstions to format/2 to include cursor position and clearing
     sections of the screen.
  3. A generic predicate to build simple menus.

@bug    The stream information on the terminal related  streams
        is not maintained by these predicates.
*/

%!  tty_clear
%
%   Clear the display.

tty_clear :-
    string_action(cl).

%!  tty_flash
%
%   Give visual signal if possible, otherwise beep.

tty_flash :-
    tty_get_capability(vb, string, Vb),
    !,
    tty_put(Vb, 1).
tty_flash :-
    put(7).

%!  string_action(+Name)
%
%   Send string from the termcap library with specified name.

string_action(Name) :-
    tty_get_capability(Name, string, String),
    tty_put(String, 1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                 FORMAT

The functions below add some extras to the format facilities.  This to
simplify screen management.  It adds ~T to the set of format characters.
The argument to ~T is a (list of) tty control commands.  The ~l command
is defined to clear to the end of the line before generating a newline.

Example:

?- format('~T~3l', home),
   format('    1) Hello World~l'),
   format('    2) Exit~2l'),
   format('    Your choice? ~T', [clear_display, flush]),
   get_single_char(X).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- format_predicate('T', tty_action(_Arg, _What)).
:- format_predicate('l', tty_nl(_Args)).

tty_action(_, What) :-
    tty_action(What).

tty_action([]) :- !.
tty_action([A|B]) :-
    !,
    tty_action(A),
    tty_action(B).
tty_action(goto(X,Y)) :-
    !,
    tty_goto(X, Y).
tty_action(home) :-
    !,
    tty_goto(0, 0).
tty_action(flush) :-
    !,
    ttyflush.
tty_action(center(Text)) :-
    !,
    tty_size(W, _),
    format('~t~a~t~*|', [Text, W]).
tty_action(back(N)) :-
    !,
    forall(between(1, N, _), put_code(8)).
tty_action(Long) :-
    abbreviation(Long, Short),
    !,
    string_action(Short).
tty_action(Short) :-
    string_action(Short).

abbreviation(clear,             cl).            % clear and home
abbreviation(clear_line,        ce).            % clear-to-end-of-line
abbreviation(clear_display,     cd).            % clear-to-end-of-display

tty_nl(default) :-
    !,
    tty_nl(1).
tty_nl(N) :-
    tty_get_capability(ce, string, Ce),
    forall(between(1, N, _),
           (   tty_put(Ce, 1),
               nl)).


                 /*******************************
                 *             MENU             *
                 *******************************/

%!  menu(+Title, +Options, -Choice) is semidet.
%
%   Show a menu. The display is cleared,   the  title is centered at
%   the top, the options are displayed  and finally the user actions
%   are parsed and the user's choice   is returned. The screen looks
%   like this:
%
%   ==
%           --------------------------------------------
%           |                                          |
%           |                  Title                   |
%           |                                          |
%           |   1) Option One                          |
%           |   2) Option Two                          |
%           |   3) Quit                                |
%           |                                          |
%           |   Your Choice? *                         |
%           |                                          |
%   ==
%
%   The user selects an item by pressing the number of the item, or
%   the first letter of the option. If more then one option match,
%   the common prefix of the matching options is given and the user
%   is expected to type the next character.  On illegal input the
%   screen is flashed (or a beep is given if the terminal can't flash
%   the screen).
%
%   Text fields (the title and option texts) are either plain atoms
%   or terms Fmt/Args.  In the latter case the argument is transformed
%   into an atom using format/3.
%
%   The specification of an option is a term PrologName:UserName.
%   PrologName is an atom, which is returned as choice if the user
%   selects this menu item.  UserName is processed as a text field
%   (see above) and displayed.  The entries are numbered automatically.
%
%   The example above could be defined as:
%
%   ==
%   get_action(Choice) :-
%           menu('Title',
%                   [ option_1 : 'Option One'
%                   , option_2 : 'Option Two'
%                   , quit     : 'Quit'
%                   ], Choice).
%   ==


menu(Title, List, Choice) :-
    show_title(Title),
    build_menu(List),
    get_answer(List, Choice).

show_title(Title) :-
    to_text(Title, T),
    format('~T~l~T~2l', [clear, center(T)]).

build_menu(List) :-
    build_menu(List, 1),
    format('~2n      Your choice? ~T', clear_display).

build_menu([], _).
build_menu([_:H|T], N) :-
    to_text(H, TH),
    format('~t~d~6|) ~a~l', [N, TH]),
    succ(N, NN),
    build_menu(T, NN).

to_text(Fmt/Args, Text) :-
    !,
    format(string(Text), Fmt, Args).
to_text(Text, Text).

:- dynamic
    menu_indent/1.

menu_indent(Old, New) :-
    (   retract(menu_indent(Old0))
    ->  Old = Old0
    ;   Old = 0
    ),
    assert(menu_indent(New)).

get_answer(List, Choice) :-
    menu_indent(_, 0),
    get_answer(List, [], Choice).

get_answer(List, Prefix, Choice) :-
    get_single_char(A),
    process_answer(A, List, Prefix, NewPrefix, Ch, Ok),
    (   Ok == yes
    ->  Ch = Choice
    ;   get_answer(List, NewPrefix, Choice)
    ).

process_answer(127, _, _, [], _, no) :-
    !,
    feedback('').
process_answer(D, List, _, _, Choice, yes) :-
    code_type(D, digit),
    name(N, [D]),
    nth1(N, List, Choice:Name),
    !,
    feedback(Name).
process_answer(D, _, _, [], _, no) :-
    code_type(D, digit),
    feedback(''),
    tty_flash.
process_answer(C, List, Prefix, NewPrefix, Choice, Ok) :-
    append(Prefix, [C], NPrefix),
    matching(List, NPrefix, Matching),
    (   Matching == []
    ->  tty_flash,
        NewPrefix = Prefix,
        Ok = no
    ;   Matching = [Choice:Name]
    ->  Ok = yes,
        feedback(Name)
    ;   common_prefix(Matching, NewPrefix),
        feedback(NewPrefix),
        Ok = no
    ).

matching([], _, []).
matching([H|T], Prefix, [H|R]) :-
    prefix(Prefix, H),
    !,
    matching(T, Prefix, R).
matching([_|T], Prefix, R) :-
    matching(T, Prefix, R).

prefix(Prefix, _:Name) :-
    name(Name, Chars),
    common_prefix_strings(Prefix, Chars, Prefix),
    !.

common_prefix([_:Name|T], Prefix) :-
    name(Name, Chars),
    common_prefix(T, Chars, Prefix).

common_prefix([], Prefix, Prefix).
common_prefix([_:Name|T], Sofar, Prefix) :-
    name(Name, Chars),
    common_prefix_strings(Chars, Sofar, NewSofar),
    common_prefix(T, NewSofar, Prefix).

common_prefix_strings([H1|T1], [H2|T2], [H1|R]) :-
    code_type(Lower, to_lower(H1)),
    code_type(Lower, to_lower(H2)),
    !,
    common_prefix_strings(T1, T2, R).
common_prefix_strings(_, _, []).

feedback(Text) :-
    atomic(Text),
    !,
    atom_length(Text, New),
    menu_indent(Old, New),
    format('~T~a~T', [back(Old), Text, clear_line]).
feedback(Text) :-
    length(Text, New),
    menu_indent(Old, New),
    format('~T~s~T', [back(Old), Text, clear_line]).
