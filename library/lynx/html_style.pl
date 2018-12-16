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

:- module(format_style,
          [ element_css/3,             % +El, +Attrs, -CSS
            css_block_options/5,       % +CSS, +M0, -Margins, -ParOptions, -Style
            css_inline_options/3,      % +CSS, -Margins, -Style
            attrs_classes/2,           % +Attrs, -Classes
            style_css_attrs/2          % +Style, -Properties
          ]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(option)).

:- multifile
    html_text:style/3.

%!  element_css(+El, +Attrs, -CSS) is semidet.

element_css(El, Attrs, CSS) :-
    findall(CSSs, applicable_style(El, Attrs, CSSs), CssList),
    CssList \== [],
    append(CssList, CSS0),
    list_to_set(CSS0, CSS).

applicable_style(_, Attrs, CSS) :-
    memberchk(style=Style, Attrs),
    style_css_attrs(Style, CSS),
    text_style(CSS).
applicable_style(El, Attrs, CSS) :-
    html_text:style(El, Cond, CSS),
    (   eval(Cond, Attrs)
    ->  true
    ).

eval(true, _).
eval(class(Class), Attrs) :-
    attrs_classes(Attrs, Classes),
    memberchk(Class, Classes).

attrs_classes(Attrs, Classes) :-
    memberchk(class=Spec, Attrs),
    split_string(Spec, " \t\r\n", " \t\r\n", ClassStrings),
    maplist(atom_string, Classes, ClassStrings).

%!  style_css_attrs(+Style, -CSS:list) is det.
%
%   Convert a style description into a list Property(Value).
%
%   @bug: far too simple.

style_css_attrs(Style, CSS) :-
    split_string(Style, ";", " \t\r\n", Parts),
    convlist(style_css_attr, Parts, CSS).

style_css_attr(Style, CSS) :-
    split_string(Style, ":", " \t\r\n", [NameS,ValueS]),
    atom_string(Name, NameS),
    atom_string(Value, ValueS),
    CSS =.. [Name,Value].

text_style(float(right)).

%!  css_block_options(+CSS, +Margins0, -Margins, -ParOptions, -Style)
%
%

css_block_options(CSS, Top0-Bottom0, Top-Bottom, ParOptions, Style) :-
    option(margin_top(Top), CSS, Top0),
    option(margin_bottom(Bottom), CSS, Bottom0),
    convlist(par_option, CSS, ParOptions),
    convlist(font_style, CSS, Style).

par_option(text_align(Align),   text_align(Align)).
par_option(margin_left(Align),  margin_left(Align)).
par_option(margin_right(Align), margin_right(Align)).

font_style(font_weight(bold),     bold).
font_style(font_weight(normal),   normal).
font_style(color(BC),             hfg(C)) :- atom_concat(bright_, C, BC).
font_style(color(C),              fg(C)).
font_style(background(BC),        hbg(C)) :- atom_concat(bright_, C, BC).
font_style(background(C),         bg(C)).
font_style(text_decoration(none), underline(false)).

%!  css_inline_options(+CSS, -Margins, -Style)
%
%

css_inline_options(CSS, Left-Right, Style) :-
    option(margin_left(Left), CSS, 0),
    option(margin_right(Right), CSS, 0),
    convlist(inline_style, CSS, Style).

inline_style(CSS, Style) :-
    font_style(CSS, Style),
    !.
inline_style(float(right),          float(right)).
