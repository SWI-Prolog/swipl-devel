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


:- module(pce_print_graphics, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- use_module(library(pce_shell)).

:- pce_begin_class(print_graphics, template,
		   "Template defining ->print").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Print  the image to the default  printer.  Also this  method should be
extended by requesting additional parameters from the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print(Canvas) :->
	"Send to default printer"::
	print_canvas(Canvas).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There are two routes to print.  On   MS-Windows  printing is achieved by
drawing on a GDI representing a printer, after which the Windows printer
driver creates printer-codes and sends them to the printer. The standard
Windows print dialog is shown by   win_printer->setup. Next we need some
calculation effort to place our diagram reasonably on the page.

In the Unix world, things go different. In general you make a PostScript
file and hand this  to  the   print-spooler,  which  will  translate the
device-independant PostScript to whatever the printer needs.

XPCE doesn't (yet)  try  to  hide   the  difference  between  these  two
approaches.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_canvas(Canvas) :-			% MS-Windows
	get(@pce, convert, win_printer, class, _), !,
	get(Canvas, default_file, Job),
	new(Prt, win_printer(Job)),
	send(Prt, setup, Canvas),
	send(Prt, open),
	get(Canvas, bounding_box, area(X, Y, W, H)),
	get(@display, dots_per_inch, size(DX, DY)),
	InchW is W/DX,
	InchH is H/DY,

	get(Prt, size, size(PW0, PH0)),
	get(Prt, dots_per_inch, size(RX, RY)),
	MarX is RX,			% default 1 inch margins
	MarY is RY,
	PrInchW is (PW0-MarX*2)/RX,
	PrInchH is (PH0-MarY*2)/RY,

	send(Prt, map_mode, isotropic),
	(   InchW < PrInchW,
	    InchH < PrInchH		% it fits on the page
	->  OX is MarX + ((PrInchW-InchW)/2)*RX,
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(OX, MarY, RX, RY))
	;   Aspect is min(PrInchW/InchW, PrInchH/InchH),
	    ARX is integer(Aspect*RX),
	    ARY is integer(Aspect*RY),
	    send(Prt, window, area(X, Y, DX, DY)),
	    send(Prt, viewport, area(MarX, MarY, ARX, ARY))
	),
	send(Prt, draw_in, Canvas?graphicals),
	send(Prt, close),
	free(Prt).
print_canvas(Canvas) :-			% Unix/PostScript
	get(Canvas, default_printer, Printer),
	default_printer(Printer),
	new(PsFile, file),
	send(PsFile, open, write),
	send(PsFile, append, Canvas?postscript),
	send(PsFile, append, 'showpage\n'),
	send(PsFile, close),
	get(PsFile, absolute_path, File),
	get(Canvas, print_command_template, CmdTempl),
	print_cmd(CmdTempl, Printer, File, Cmd),
	pce_shell_command('/bin/sh'('-c', Cmd)),
	send(PsFile, remove),
	send(PsFile, done),
	send(Canvas, report, status, 'Sent to printer `%s''', Printer).
	

default_printer(Canvas, Printer:name) :<-
	"Get name of the printer"::
	get(Canvas, frame, Frame),
	default_printer(DefPrinter),
	new(D, dialog(select_printer?label_name)),
	send(D, append, new(P, text_item(printer, DefPrinter))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, append, button(ok, message(D, return, P?selection))),
	send(D, default_button, ok),
	send(D, transient_for, Frame),
	send(D, modal, transient),
	get(D, confirm_centered, Canvas?frame?area?center, Answer),
	send(D, destroy),
	Answer \== @nil,
	Printer = Answer.

default_printer(Printer) :-
	get(@pce, environment_variable, 'PRINTER', Printer), !.
default_printer(postscript).


print_job_name(_, Job) :<-
	"Default name of the printer job"::
	Job = 'XPCE/SWI-Prolog'.
print_command_template(_, Command) :<-
	"Default command to send a job to the printer"::
	Command = 'lpr -P%p "%f"'.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print_cmd(+Template, +Printer, +File,  -Command)   determines  the shell
command to execute in order to get `File' printed on `Printer' using the
given template. The substitutions are handled by a regex object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_cmd(Template, Printer, File, Cmd) :-
	new(S, string('%s', Template)),
	substitute(S, '%p', Printer),
	substitute(S, '%f', File),
	get(S, value, Cmd),
	free(S).

substitute(S, F, T) :-
	new(R, regex(F)),
	send(R, for_all, S,
	     message(@arg1, replace, @arg2, T)),
	free(R).


:- pce_end_class(print_graphics).
	  
