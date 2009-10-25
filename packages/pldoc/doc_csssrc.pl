/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc_csssrc,
	  [ write_source_css/0,		% Create pllisting.css
	    write_source_css/1		% +Stream
	  ]).
:- use_module(library(pce)).
:- use_module(doc_htmlsrc).
:- use_module(doc_colour).


%%	write_source_css is det.
%%	write_source_css(+Out:stream) is det.
%
%	Create   a   style-sheet   from    the   style-declarations   in
%	doc_colour.pl    and    the    element     declaration    above.
%	write_source_css/0 writes the style-sheet to =|pllisting.css|=.

:- op(990, xfx, :=).

write_source_css :-
	open('pllisting.css', write, Out),
	call_cleanup(write_source_css(Out),
		     close(Out)).

write_source_css(Out) :-
	(   prolog_src_style(Term, Style0),
	    (	html_style(Term, Style)
	    ->	true
	    ;	Style = Style0
	    ),
	    pldoc_htmlsrc:element(Term2, Tag, Class),
	    Term2 =@= Term,
	    findall(Name=Value, style_attr(Style, Name, Value),
		    [N=V|NV]),
	    format(Out, '~w.~w~n', [Tag, Class]),
	    format(Out, '{ ~w: ~w;~n', [N, V]),
	    forall(member(N2=V2, NV),
		   format(Out, '  ~w: ~w;~n', [N2, V2])),
	    format(Out, '}~n~n', []),
	    fail
	;   true
	).

style_attr(Style, Name, Value) :-
	arg(_, Style, PceName := PceValue),
	pce_to_css_attr(PceName, Name),
	pce_to_css_value(Name, PceValue, Value).

pce_to_css_attr(colour, color).
pce_to_css_attr(background, 'background-color').
pce_to_css_attr(underline, 'text-decoration').
pce_to_css_attr(bold, 'font-weight').
pce_to_css_attr('font-style', 'font-style').
pce_to_css_attr(display, display).

pce_to_css_value(color, Name, RGB) :-
	x11_colour_name_to_rgb(Name, RGB).
pce_to_css_value('background-color', Name, RGB) :-
	x11_colour_name_to_rgb(Name, RGB).
pce_to_css_value('text-decoration', @(on), underline).
pce_to_css_value('font-weight', @(on), bold).
pce_to_css_value('font-style', Style, Style).

x11_colour_name_to_rgb(red, red) :- !.
x11_colour_name_to_rgb(blue, blue) :- !.
x11_colour_name_to_rgb(Name, RGB) :-
	get(@(pce), convert, Name, colour, Obj),
	get(Obj, red, R),
	get(Obj, green, G),
	get(Obj, blue, B),
	R256 is R//256,
	G256 is G//256,
	B256 is B//256,
	format(atom(RGB),
	       '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+',
	       [R256, G256, B256]).

%%	html_style(+Term, -Style) is semidet.
%
%	Redefine styles from prolog_src_style/2 for better ones on
%	HTML output.

html_style(var,
	   style(colour := red4,
		 'font-style' := italic)).
html_style(directive,
	   style(background := grey90,
		 'display' := block)).
