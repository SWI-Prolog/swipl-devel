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

:- module(pce_print_text, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module extends class text_buffer with   the ->print method, dealing
with printing plain text.  This  issue   is  completely  different  when
comparing Windows and Unix. 

Windows
=======

We create an editor and print the image thereof on a win_printer, page
by page.

Unix
====

There are many options here, unfortunately   none  is very standard. You
can pipe the output directly through the   lpr  command, use an external
beautifier that creates nice-looking  PostScript,   such  as  nenscript,
mpage or aps or create the  PostScript   yourself.  At the moment, class
text_image does not provide for generating   PostScript,  so this is not
yet an option. To keep things simple we   pipe through lpr and leave the
details to the lpr installation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(text_buffer).

print(TB, From:from=[int], To:to=[int],
      JobName:job=[name],
      Editor:editor=[editor],
      Font:font=[font]) :->
	"Print (region of) the text-buffer"::
	get(TB, size, Size),
	default(From, 0, F),
	default(To, Size, T),
	(   get(@pce, convert, win_printer, class, _)
	->  send(TB, win_print, F, T, JobName, Editor, Font)
	;   send(TB, unix_print, F, T, JobName, Editor, Font)
	).

		 /*******************************
		 *	 WINDOWS PRINTING	*
		 *******************************/

win_print(TB, From:int, To:int,
	  JobName:[name], Editor:[editor], Font:[font]) :->
	new(Prt, win_printer(JobName)),
	(   Editor \== @default,
	    get(Editor, frame, Frame)
	->  true
	;   Frame = @default
	),
	send(Prt, setup, Frame),
	(   send(Prt, open)
	->  true
	;   send(TB, report, error, 'Failed to open printer'),
	    fail
	),
	new(E, editor(TB, 80)),
	send(E?image, elevation, @nil),
	send(E?image, pen, 0),
	send(TB, setup_print_editor, E, Editor),
	(   Font \== @default
	->  send(E, font, Font)
	;   true
	),
	get(E?area, width, Width),
	get(Prt, size, size(W,H)),
	get(Prt, dots_per_inch, size(DPIX,DPIY)),
	InchW is W/DPIX,
	InchH is H/DPIY,
	PageW is round(Width*(InchW/(InchW-2))), % 1 inch margin
	PageH is round(PageW*(H/W)),
	send(Prt, resolution, PageH),
	Height is round(PageH*((InchH-2)/InchH)),
	LDPI is round(PageH/InchH),
	send(E, do_set, 0, 0, Width, Height),

	new(BG, device),		% background
	(   JobName \== @default
	->  send(BG, display, new(T, text(JobName))),
	    send(BG, display, new(P, text)),
	    send(P, name, pageno),
	    get(T, height, TH),
	    BT is LDPI-TH/2,
	    Right is LDPI+Width,
	    send(T, position, point(LDPI, BT-TH)),
	    send(P, position, point(Right, BT-TH)),
	    send(BG, display, line(LDPI, BT, Right, BT)),
	    send(BG, attribute, page_right, Right)
	;   true
	),
	
	send(E, scroll_to, From, 1),
	print_pages(Prt, E, To, margin(LDPI,LDPI), BG, 1, Pages),
	send(TB, report, status, 'Sent %d pages to the printer', Pages),
	send(Prt, close),
	send(E, destroy).
	
setup_print_editor(_TB, E:editor, From:[editor]) :->
	"Get defaults from editor"::
	(   From \== @default
	->  get(From, styles, Styles),
	    send(Styles, for_all,
		 message(E, style, @arg1?name, @arg1?value)),
	    get(From, font, Font),
	    send(E, font, Font)
	;   true
	).

%	print_pages(+Printer, +Editor, +EndIndex, +margin(X,Y),
%		    +Background, +Page, -LastPage)
%		    
%       Actual page printing loop.

print_pages(Printer, Editor, End, Margin, BG, Page, Pages) :-
	get(Editor, image, Image),
	format('Printing page~n'),
	Margin = margin(MX, MY),
	(   get(BG, member, pageno, PageNoText)
	->  send(PageNoText, string, string('%s %d', page?label_name, Page)),
	    get(BG, page_right, Right),
	    send(PageNoText, x, Right-PageNoText?width)
	;   true
	),
	send(Printer, draw_in, BG),
	get(Image, end, EndImg),
	(   EndImg > End
	->  get(Image, character_position, End, point(_,BaseY)),
	    format('BaseY = ~w~n', [BaseY]),
	    send(Editor, do_set, height := BaseY)
	;   true
	),
	send(Printer, draw_in, Image, point(MX, MY)),
	format('Ok, done upto ~w~n', [EndImg]),
	(   (   EndImg >= End
	    ;	get(Image, eof_in_window, @on)
	    )
	->  Pages = Page
	;   send(Printer, next_page),
	    format('Next page from ~w~n', [EndImg]),
	    send(Editor, scroll_to, EndImg, 1),
	    NextPage is Page+1,
	    print_pages(Printer, Editor, End, Margin, BG, NextPage, Pages)
	).


		 /*******************************
		 *	   UNIX PRINTING	*
		 *******************************/

unix_print(TB, From:int, To:int,
	   _JobName:[name], Editor:[editor], _Font:[font]) :->
	"Print on Unix printer"::
	(   getenv('PRINTER', LP)
	->  true
	;   LP = lp
	),
	atom_concat('lpr -P', LP, DefCommand),
	new(D, dialog(print_command?label_name)),
	send(D, append, new(TI, text_item(command, DefCommand))),
	send(D, append, button(print, message(D, return, TI?selection))),
	send(D, append, button(cancel, message(D, destroy))),
	send(D, default_button, print),
	(   Editor \== @default,
	    get(Editor, frame, Frame)
	->  send(D, transient_for, Frame),
	    send(D, modal, transient),
	    get(Frame?area, center, Center)
	;   Center = @default
	),
	get(D, confirm_centered, Center, Command),
	send(D, destroy),
	get(TB, contents, From, To-From, String),
	get(String, value, Atom),
	catch(do_print(Command, Atom), E,
	      (	  message_to_string(E, Msg),
		  send(TB, report, error, Msg))),
	send(TB, report, status, 'Document sent to printer').

do_print(Command, Text) :-
	open(pipe(Command), write, Out),
	write(Out, Text),
	close(Out).
	    
:- pce_end_class.

/*
test :-
	new(TB, text_buffer),
	send(TB, insert_file, 0, 'print_text.pl'),
	send(TB, try_print, job := 'Test me').

test(To) :-
	new(TB, text_buffer),
	send(TB, insert_file, 0, 'print_text.pl'),
	send(TB, try_print, to := To, job := 'Test me').
*/
