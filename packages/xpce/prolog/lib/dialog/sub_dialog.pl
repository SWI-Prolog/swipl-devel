/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_sub_dialog, []).
:- use_module(library(pce)).

:- pce_begin_class(sub_dialog, figure).

		 /*******************************
		 *	     FILLING		*
		 *******************************/

append(SD, Gr:graphical, Where:[name]) :->
	send(SD, append_dialog_item, Gr, Where).

layout(SD) :->
	send(SD, layout_dialog).


		 /*******************************
		 *	      TYPING		*
		 *******************************/

'_wants_keyboard_focus'(_) :->
	true.

:- pce_global(@compound_dialog_recogniser,
	      new(handler_group(handler(obtain_keyboard_focus,
					message(@receiver, advance))))).

event(D, Ev:event) :->
	(   send(@compound_dialog_recogniser, event, Ev)
	->  true
	;   send(D, send_super, event, Ev)
	).

:- pce_end_class.
