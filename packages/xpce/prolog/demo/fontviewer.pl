/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_fontviewer,
	  [ fontviewer/0
	  ]).
:- use_module(library(pce)).
:- require([ between/3
	   ]).

fontviewer :-
	new(FontViewer, frame('Font Viewer')),
	send(FontViewer, append, new(B, browser(size := size(35, 10)))),
	send(FontViewer, append, new(D, dialog)),
	send(FontViewer, append, new(P, picture(size := size(350,350)))),
	send(P, right, B),
	send(D, below, B),

	send(D, append,
	     new(Open, button(open, message(@prolog, show_font,
					    P, B?selection?object)))),
	send(D, append,
	     button(quit, message(FontViewer, destroy))),
	send(D, append, label(reporter), right),
	send(D, default_button, open),

	send(B, tab_stops, vector(80, 180)),
	send(B, open_message, message(Open, execute)),
	send(FontViewer, open),

	new(FontList, chain),
	send(@fonts, for_all, message(FontList, append, @arg2)),
	send(FontList, sort,
	     ?(@prolog, compare_fonts, @arg1, @arg2)),

	send(FontList, for_all,
	     message(@prolog, append_font_browser, B, @arg1)).

compare_fonts(F1, F2, Result) :-
	get(F1?family, compare, F2?family, Result),
	Result \== equal, !.
compare_fonts(F1, F2, Result) :-
	get(F1?style, compare, F2?style, Result),
	Result \== equal, !.
compare_fonts(F1, F2, Result) :-
	get(F1?points, compare, F2?points, Result).

append_font_browser(B, Font) :-
	get(Font, family, Fam),
	get(Font, style, Style),
	get(Font, points, Points),
	get(Font, object_reference, Name),
	send(B, append, dict_item(Name,
				  string('%s\t%s\t%d', Fam, Style, Points),
				  Font)).


show_font(P, Font) :-
	send(P, clear),
	new(F, format(horizontal, 2, @on)),
	send(F, row_sep, 0),
	send(P, format, F),
	new(A, string(x)),
	(   between(0, 15, Y),
	    I is Y*16,
	    send(P, display,
		 text(string('%03o/0x%02x/%03d:', I, I, I), left, fixed)),
	    new(S, string),
	    (   between(0, 15, X),
		C is 16*Y + X,
		C \== 0, C \== 10, C \== 13,
		send(A, character, 0, C),
		send(S, append, A),
		fail
	    ;   send(P, display, font_text(S, left, Font))
	    ),
	    fail
	;   true
	).

:- pce_begin_class(font_text, text,
		   "Show current character").

event(FT, Ev:event) :->
	(   send(FT, send_super, event, Ev)
	->  true
	;   send(Ev, is_a, area_exit)
	->  send(FT, report, status, '')
	;   get(FT, pointed, Ev, Index),
	    get(FT?string, character, Index, C),
	    send(FT, report, status, '%c = %03o/0x%02x/%03d', C, C, C, C)
	).

:- pce_end_class.
