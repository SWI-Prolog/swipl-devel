/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
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
	(   current_predicate(_, send_class(_, _, _))
	->  Msg =.. [report, Status, Fmt | Args], % new xpce-5
	    send_super(R, Msg)
	;   Goal =.. [send, R, send_super, report, Status, Fmt | Args],
	    Goal
	),
	colour(Status, Colour),
	send(R, colour, Colour).
	
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
