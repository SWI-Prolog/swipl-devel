/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pce_colour_browser,
	  [ colour_browser/0
	  ]).
:- use_module(library(pce)).
:- require([ absolute_file_name/3
	   , send_list/3
	   ]).

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(x11, OpenWin) :-
	get(@pce, environment_variable, 'OPENWINHOME', OpenWin).
user:file_search_path(x11, '/usr/lib/X11').
user:file_search_path(x11, PceLib) :-
	get(@pce, window_system, windows),
	get(@pce, home, PceHome),
	atom_concat(PceHome, '/lib', PceLib).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This demo implements a  browser  for   the  X11  predefined colours. The
colour  browser  assumes  the  X11  colour    database   is  located  in
x11('lib/rgb.txt').
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

colour_browser :-
	make_colour_browser(CB),
	send(CB, open).

make_colour_browser(CB) :-
	absolute_file_name(x11(rgb),
			   [ extensions([txt]),
			     access(read),
			     file_errors(fail)
			   ], DataBase), !,
	make_colour_browser(CB, DataBase).
make_colour_browser(CB) :-
	make_colour_browser(CB, @colour_names).

make_colour_browser(CB, DataBase) :-
	new(D, dialog),
	send(D, append, new(M, menu(which, choice))),
	send_list(M, append, [foreground, background]),
	send(M, selection, background),
	send(new(CB, browser('XPCE (X11) named colours')), below, D),
	send(CB, width, 33),

	send(CB?key_binding, function,	% pretend arrow up/down not only
	     cursor_down,		% do the preview, but also show
	     and(message(CB, next_line),% the colour immediately
		 message(CB, enter))),
	send(CB?key_binding, function,
	     cursor_up,
	     and(message(CB, previous_line),
		 message(CB, enter))),

					% Make a picture with some different
					% graphicals for preview
	send(new(P, picture(size := size(100, 25))), below, CB),
	send(P, ver_stretch, 0),
	send(P, ver_shrink, 0),
	send(P, format, new(Fmt, format(vertical, 1, @on))),
	send(Fmt, adjustment, vector(center)),
	send(P, display, graphical(0, 0, 0, 25)),
	send(P, display, text(hello, left, normal)),
	send(P, display, text(hello, left, bold)),
	send(P, display, new(Box, box(30, 12))),
	send(P, display, line(0, 0, 30, 0)),
	send(Box, fill_pattern, @black_image),

					% A colour is selected; call
					% selected_colour/3, using the
					% picture, fore/back ground and
					% the description of the colour
	send(CB, select_message,
	     message(@prolog, selected_colour, P, M?selection, @arg1?label)),

					% set tab-stops for nice alignment
	send(CB?image, tab_stops, vector(110, 140, 170)),
					% scan de database
	(   send(DataBase, instance_of, hash_table)
	->  send(DataBase, for_all,
		 message(@prolog, append_colour, CB, @arg1, @arg2)),
	    send(CB, sort)
	;   new(F, file(DataBase)),
	    (   send(F, open, read)
	    ->  repeat,
		(   get(F, read_line, String)
		->  get(String, scan, '%d%d%d%*[ 	]%[a-zA-Z0-9_ ]',
			vector(R, G, B, Name)),
		    send(Name, translate, ' ', '_'),
		    send(Name, downcase),
		    get(Name, value, Atom),
		    (   get(CB, member, Atom, _)
		    ->  true
		    ;   send(CB, append,
			     dict_item(Name,
				       string('%s\t%d\t%d\t%d',
					      Name, R, G, B)))
		    ),
		    fail
		;   !,
		    send(F, close)
		)
	    )
	).

append_colour(CB, Name, RGB) :-
	B is RGB >> 16,
	G is (RGB >> 8) /\ 255,
	R is (RGB /\ 255),
	send(CB, append, string('%s\t%d\t%d\t%d', Name, R, G, B)).
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A colour was selected.  Change the colour attribute of the picture window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

selected_colour(P, Which, Label) :-
	get(Label, scan, '%s%d%d%d', vector(NameStr, R, G, B)),
	send(@display, cut_buffer, 0, NameStr),
	XR is R * 257,
	XG is G * 257,
	XB is B * 257,
	send(P, Which, colour(@default, XR, XG, XB)).
