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

:- module(pldoc_style,
          []).

:- multifile
    html_text:style/3.

html_text:style(Element, Condition, CSS) :-
    style(Element, Condition, CSS).

style(div, class(synopsis),
      [text_align(right), margin_bottom(2), margin_top(2)]).
style(div, class(title),
      [text_align(center), margin_bottom(3), margin_top(2), font_weight(bold)]).
style(div, class('abstract-title'),
      [margin_bottom(2), text_align(center), font_weight(bold)]).
style(div, class(author),
      [text_align(center), margin_bottom(3), margin_top(2)]).
style(div, class(abstract),
      [margin_bottom(3), margin_top(2)]).
style(div, class('toc-h1'),
      [margin_left(2)]).
style(div, class('toc-h2'),
      [margin_left(4)]).
style(div, class('toc-h3'),
      [margin_left(6), font_weight(normal)]).   % FIXME: Requires hierarchy
style(div, class('toc-h4'),
      [margin_left(8), font_weight(normal)]).
style(span, class('synopsis-hdr'),
      [margin_right(1), font_weight(normal), color(green)]).
style(span, class(autoload),
      [margin_left(1), font_weight(normal), color(green)]).
style(span, class('pred-tag'),
      [float(right)]).
style(var, true,
      [color(red), font_weight(normal)]).
style(code, true,
      [color(blue)]).
style(pre, true,
      [color(blue)]).
style(a, true,
      [text_decoration(none), font_weight(bold)]).
style(table, class(arglist),
      [margin_left(4), margin_right(4)]).
style(p, class(warning),
      [text_align(center), color(red)]).
style(span, class('help-query'),
      [font_weight(bold)]).
