/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(pce_reporter, []).

:- pce_begin_class(reporter, label,
		   "Label for reporting").

variable(hor_stretch,	int := 100,	get, "Stretch-ability").
variable(hor_shrink,	int := 100,	get, "Shrink-ability").

initialise(R) :->
	send(R, send_super, initialise, reporter, ''),
	send(R, elevation, -1),
	send(R, border, 2),
	send(R, reference, point(0, R?height)).

report(R, Status:name, Fmt:[char_array], Args:any ...) :->
	Msg =.. [report, Status, Fmt | Args], % new xpce-5
	colour(Status, Colour),
	send(R, colour, Colour),
	send_super(R, Msg).
	
colour(error, red) :- !.
colour(_, @default).

:- pce_end_class.


:- pce_begin_class(report_dialog, dialog,
		   "Dialog window holding reporter").

initialise(D) :->
	send_super(D, initialise),
	send(D, gap, size(0, 0)),
	send(D, resize_message, message(D, layout, @arg2)),
	send(D, append, new(reporter)).

:- pce_end_class.
