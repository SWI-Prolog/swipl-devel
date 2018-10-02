/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, CWI Amsterdam
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

:- module(text_format,
          [ format_paragraph/2          % +Text, +Options
          ]).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lists)).

/** <module> Print formatted text to a terminal

This module is the core of the   plain  text rendering module, providing
format_paragraph/2 which formats a plain text block, respecting left and
right margins, text alignment, ANSI style elements, etc.
*/

:- multifile
    words/2.                            % +Input, -Words

%!  format_paragraph(+Text, +Options)
%
%   Format a paragraph to the current output.  Options defined are:
%
%     - width(+Width)
%     Width of a line.  Default is 72.
%     - margin_left(+Indent)
%     Indent all lines with Indent spaces.
%     - margin_right(+Margin)
%     Additional right margin (same as reducing `width`)
%     - hang(+Hang)
%     Additional indent for the first line.  Can be negative.
%     - bullet(+Bullet)
%     Bullet placed before the first line.
%     - text_align(Alignment)
%     One of `left`, `right`, `center` or `justify`
%     - pad(+Char)
%     If present, padd to the right using Char. Currently
%     Char *must be* ' '.

format_paragraph(Text, Options) :-
    words(Text, Words),
    format_lines(Words, 1, Options).

format_lines([], _, _).
format_lines(Words, LineNo, Options) :-
    line_width(LineNo, Width, Options),
    skip_spaces(Words, Words1),
    take_words(Words1, 0, Width, Line0, HasBR, Words2),
    skip_trailing_spaces(Line0, Line),
    skip_spaces(Words2, Words3),
    (   Words3 == []
    ->  align_last_line(Options, OptionsLast),
        format_line(Line, Width, LineNo, OptionsLast)
    ;   HasBR == true
    ->  align_last_line(Options, OptionsLast),
        format_line(Line, Width, LineNo, OptionsLast),
        LineNo1 is LineNo + 1,
        format_lines(Words3, LineNo1, Options)
    ;   format_line(Line, Width, LineNo, Options),
        LineNo1 is LineNo + 1,
        format_lines(Words3, LineNo1, Options)
    ).

take_words([br(_)|T], _, _, [], true, T) :-
    !.
take_words([H|T0], X, W, [H|T], BR, Rest) :-
    element_length(H, Len),
    X1 is X+Len,
    (   X1 =< W
    ->  true
    ;   X == 0                          % take at least one word
    ),
    !,
    take_words(T0, X1, W, T, BR, Rest).
take_words(Rest, _, _, [], false, Rest).

:- public
    trim_spaces/2.

trim_spaces(Line0, Line) :-
    skip_spaces(Line0, Line1),
    skip_trailing_spaces(Line1, Line).

skip_spaces([b(_,_)|T0], T) :-
    !,
    skip_spaces(T0, T).
skip_spaces(L, L).

skip_trailing_spaces(L, []) :-
    skip_spaces(L, []),
    !.
skip_trailing_spaces([H|T0], [H|T]) :-
    skip_trailing_spaces(T0, T).

align_last_line(Options0, Options) :-
    select_option(text_align(justify), Options0, Options1),
    !,
    Options = [text_align(left)|Options1].
align_last_line(Options, Options).


%!  format_line(+Line, +Width, +LineNo, +Options) is det.

format_line(Line, Width, LineNo, Options) :-
    option(pad(Char), Options),
    option(margin_right(MR), Options),
    MR > 0,
    !,
    must_be(oneof([' ']), Char),        % For now
    format_line_(Line, Width, LineNo, Options),
    forall(between(1, MR, _), put_char(' ')).
format_line(Line, Width, LineNo, Options) :-
    format_line_(Line, Width, LineNo, Options).

format_line_(Line, Width, LineNo, Options) :-
    float_right(Line, Line1, Right),
    !,
    trim_spaces(Line1, Line2),                  % TBD: Alignment with floats
    trim_spaces(Right, Right2),
    space_dim(Line2, _, WL),
    space_dim(Right2, _, WR),
    append(Line2, [b(0,Space)|Right2], Line3),
    Space is Width - WL - WR,
    emit_indent(LineNo, Options),
    emit_line(Line3).
format_line_(Line, Width, LineNo, Options) :-
    option(text_align(justify), Options),
    !,
    justify(Line, Width),
    emit_indent(LineNo, Options),
    emit_line(Line).
format_line_(Line, Width, LineNo, Options) :-
    option(text_align(right), Options),
    !,
    flush_right(Line, Width, LineR),
    emit_indent(LineNo, Options),
    emit_line(LineR).
format_line_(Line, Width, LineNo, Options) :-
    option(text_align(center), Options),
    option(pad(Pad), Options, _),
    !,
    center(Line, Width, Pad, LineR),
    emit_indent(LineNo, Options),
    emit_line(LineR).
format_line_(Line, Width, LineNo, Options) :-
    option(pad(_Char), Options),
    !,
    pad(Line, Width, Padded),
    emit_indent(LineNo, Options),
    emit_line(Padded).
format_line_(Line, _Width, LineNo, Options) :-
    emit_indent(LineNo, Options),
    emit_line(Line).

justify(Line, Width) :-
    space_dim(Line, Spaces, W0),
    Spread is Width - W0,
    length(Spaces, SPC),
    SPC > 0,
    Spread > 0,
    spread(Spread, SPC, Spaces),
    !,
    debug(format(justify), 'Justified ~d spaces over ~d gaps: ~p',
          [Spread, SPC, Spaces]).
justify(_, _).

flush_right(Line, Width, [b(0,Spaces)|Line]) :-
    space_dim(Line, _Spaces, W0),
    Spaces is Width - W0.

center(Line, Width, Pad, [b(0,Left)|Padded]) :-
    space_dim(Line, _Spaces, W0),
    Spaces is Width - W0,
    Left is Spaces//2,
    (   atom(Pad),
        Right is Spaces - Left,
        Right > 0
    ->  append(Line, [b(0,Right)], Padded)
    ;   Padded = Line
    ).

pad(Line, Width, Padded) :-
    space_dim(Line, _Spaces, W0),
    Spaces is Width - W0,
    append(Line, [b(0,Spaces)], Padded).


%!  float_right(+Line0, -Line, -Right) is semidet.
%
%

float_right(Line0, Line, Right) :-
    member(w(_,_,Attrs), Line0),
    memberchk(float(right), Attrs),
    !,
    do_float_right(Line0, Line, Right).

do_float_right([], [], []).
do_float_right([H0|T0], T, [H|R]) :-
    float_right_word(H0, H),
    !,
    float_right_space(T0, T, R).
do_float_right([H|T0], [H|T], R) :-
    do_float_right(T0, T, R).

float_right_word(w(W,L,A0), w(W,L,A)) :-
    selectchk(float(right), A0, A).

float_right_space([S|T0], T, [S|R]) :-
    S = b(_,_),
    !,
    float_right_space(T0, T, R).
float_right_space(Line, Line, []).


%!  space_dim(+Line, -SpaceVars, -Width)

space_dim(Line, Spaces, Width) :-
    space_dim(Line, Spaces, 0, Width).

space_dim([], [], Width, Width).
space_dim([b(L,Var)|T0], [Var|T], W0, W) :-
    !,
    W1 is W0+L,
    space_dim(T0, T, W1, W).
space_dim([H|T0], T, W0, W) :-
    word_length(H, L),
    !,
    W1 is W0+L,
    space_dim(T0, T, W1, W).

%!  spread(+Spread, +SPC, -Spaces)
%
%   Distribute Spread spaces over  SPC  places,   producing  a  list  of
%   counts.

spread(Spread, SPC, Spaces) :-
    spread_spc(SPC, Spread, Spaces).

spread_spc(Cnt, Spread, [H|T]) :-
    Cnt > 0,
    !,
    H is round(Spread/Cnt),
    Cnt1 is Cnt - 1,
    Spread1 is Spread-H,
    spread_spc(Cnt1, Spread1, T).
spread_spc(_, _, []).

%!  emit_line(+Content)
%

emit_line([]).
emit_line([H|T]) :-
    (   emit_line_element(H)
    ->  true
    ;   type_error(line_element, H)
    ),
    emit_line(T).

emit_line_element(w(W,_, Attrs)) :-
    (   Attrs = []
    ->  write(W)
    ;   ansi_format(Attrs, '~w', [W])
    ).
emit_line_element(b(Len, Extra)) :-
    (   var(Extra)
    ->  Extra = 0
    ;   true
    ),
    Spaces is Len+Extra,
    forall(between(1, Spaces, _), put_char(' ')).

emit_indent(1, Options) :-
    !,
    option(margin_left(Indent), Options, 0),
    option(hang(Hang), Options, 0),
    (   option(bullet(BulletSpec), Options)
    ->  bullet_text(BulletSpec, Bullet),
        atom_length(Bullet, BLen),
        TheIndent is Indent+Hang-1-BLen,
        emit_indent(TheIndent),
        format('~w ', [Bullet])
    ;   TheIndent is Indent+Hang,
        emit_indent(TheIndent)
    ).
emit_indent(_, Options) :-
    option(margin_left(Indent), Options, 0),
    nl,
    emit_indent(Indent).

emit_indent(N) :-
    forall(between(1, N, _),
           put_char(' ')).

line_width(1, Width, Options) :-
    !,
    option(width(Right), Options, 72),
    option(margin_left(Indent), Options, 0),
    option(margin_right(RightMargin), Options, 0),
    option(hang(Hang), Options, 0),
    Width is Right - (Indent+Hang) - RightMargin.
line_width(_, Width, Options) :-
    option(width(Right), Options, 72),
    option(margin_left(Indent), Options, 0),
    option(margin_right(RightMargin), Options, 0),
    Width is Right - Indent - RightMargin.

%!  words(+Input, -Words) is det.
%
%   Turn the Input into a list of w(Word, Len, Attributes) terms.

words(Text, Words) :-
    string(Text),
    !,
    split_string(Text, " \n\t\r", " \n\t\r", Words0),
    phrase(word_spaces(Words0), Words).
words(Words, Words) :-
    is_list(Words),
    !.

word_spaces([]) -->
    [].
word_spaces([""]) -->
    !.
word_spaces([H|T]) -->
    { string_length(H, Len) },
    [ w(H, Len, []) ],
    (   {T==[]}
    ->  []
    ;   [b(1,_)],
        word_spaces(T)
    ).

word_length(w(_,Len,_), Len).

element_length(w(_,Len,_), Len).
element_length(b(Len,_), Len).

bullet_text(I, Bullet) :-
    integer(I),
    !,
    format(string(Bullet), '~d.', [I]).
bullet_text(Bullet, Bullet).
