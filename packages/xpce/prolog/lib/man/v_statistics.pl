/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/



:- module(pce_statistics, []).

:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

:- pce_begin_class(man_statistics, man_frame,
		   "Statistics tool").


variable(timer, timer*,	get,
	 "Timer that forces updates").

initialise(S, Manual:man_manual) :->
	"Create from manual"::
	send(S, send_super, initialise, Manual, 'PCE Statistics'),
	
	send(S, append, new(D, dialog)),
	send(D, append, new(CU, text_item(core_in_use,    0))),
	send(D, append, new(CW, text_item(core_wasted,    0))),
	send(D, append, new(OU, text_item(objects_in_use, 0))),
	Items = [CU, CW, OU],
	send_list(Items, pen, 0),
	send_list(Items, length, 10),
	send_list(Items, editable, @off),

	new(T, timer(5,
		     block(message(CU, selection, @pce?core_usage),
			   message(CW, selection, @pce?core_wasted),
			   message(OU, selection, @pce?objects_allocated -
				   		  @pce?objects_freed)))),
	send(S, slot, timer, T),
	send(T, execute),
	send(T, start),

	send(D, append, button(update, message(T, execute))),
	send(D, append, button(help,   message(S, help))),
	send(D, append, button(quit,   message(S, quit))),

	send(S, open).


unlink(S) :->
	send(S?timer, stop),
	send(S, slot, timer, @nil),
	send(S, send_super, unlink).

:- pce_end_class.
