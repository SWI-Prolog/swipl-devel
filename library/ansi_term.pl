/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

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

:- module(ansi_term,
	  [ ansi_format/3		% +Attr, +Format, +Args
	  ]).
:- use_module(library(apply)).

/** <module> Print decorated text to ANSI consoles

This library allows for exploiting the color and attribute facilities of
most modern terminals using ANSI escape sequences.

@see	http://en.wikipedia.org/wiki/ANSI_escape_code
@bug	The Windows console (swipl-win) does not (yet) support ANSI (color)
	codes.
*/

:- create_prolog_flag(color_term, true, [type(boolean)]).

%%	ansi_format(+Attributes, +Format, +Args) is det.
%
%	Format text with ANSI  attributes.   This  predicate  behaves as
%	format/2 using Format and Args, but if the =current_output= is a
%	terminal, it adds ANSI escape sequences according to Attributes.
%	For example, to print a text in bold cyan, do
%
%	  ==
%	  ?- ansi_format([bold,fg(cyan)], 'Hello ~w', [world]).
%	  ==
%
%	Attributes is either a single attribute   or a list thereof. The
%	attribute names are derived from the ANSI specification. See the
%	source for sgr_code/2 for details. Some commonly used attributes
%	are:
%
%	  - bold
%	  - underline
%	  - fg(Color), bg(Color), hfg(Color), hbg(Color)
%
%	Defined color constants are below.  =default=   can  be  used to
%	access the default color of the terminal.
%
%	  - black, red, green, yellow, blue, magenta, cyan, white
%
%	ANSI sequences are omitted if and only if
%
%	  - The =current_output= has the property tty(true) (see
%	    stream_property/2).
%	  - The Prolog flag =color_term= is =true=.

ansi_format(Attr, Format, Args) :-
	ansi_format(current_output, Attr, Format, Args).

ansi_format(Stream, Attr, Format, Args) :-
	stream_property(Stream, tty(true)),
	current_prolog_flag(color_term, true), !,
	(   is_list(Attr)
	->  maplist(sgr_code, Attr, Codes),
	    atomic_list_concat(Codes, ;, Code)
	;   sgr_code(Attr, Code)
	),
	(   Args == []
	->  format(Stream, '\u001B[~wm~w\u001B[0m', [Code, Format])
	;   format(string(Fmt), '\u001B[~~wm~w\u001B[0m', [Format]),
	    format(Stream, Fmt, [Code|Args])
	).
ansi_format(Stream, _Attr, Format, Args) :-
	format(Stream, Format, Args).

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
sgr_code(franktur, 20).
sgr_code(underline(double), 21).
sgr_code(intensity(normal), 22).
sgr_code(fg(Name), C) :-
	ansi_color(Name, N),
	C is N+30.
sgr_code(bg(Name), C) :- !,
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
sgr_code(hbg(Name), C) :- !,
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
ansi_color(red,	    1).
ansi_color(green,   2).
ansi_color(yellow,  3).
ansi_color(blue,    4).
ansi_color(magenta, 5).
ansi_color(cyan,    6).
ansi_color(white,   7).
ansi_color(default, 9).


		 /*******************************
		 *	       HOOK		*
		 *******************************/

%%	prolog:message_line_element(+Stream, +Term) is semidet.
%
%	Hook implementation that deals with  ansi(+Attr, +Fmt, +Args) in
%	message specifications.

prolog:message_line_element(S, ansi(Attr, Fmt, Args)) :-
	ansi_format(S, Attr, Fmt, Args).
