/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(dialog,
	  [ dialog/1
	  ]).

dialog(W) :-
	new(D, dialog('Edit Window Attributes')),
	send(D, append, label(reporter)),
	send(D, append,
	     text_item(frame_label,
		       W?frame?label,
		       message(W?frame, label, @arg1))),
	send(D, append,
	     slider(width, 50, 500, W?visible?width,
		    message(W, width, @arg1))),
	send(D, append,
	     slider(height, 50, 500, W?visible?height,
		    message(W, height, @arg1))),
	send(D, append, button(apply)),
	send(D, append, button(restore)),
	send(D, append, button(quit, message(D, destroy))),
	send(D, default_button, apply),
	send(D, open).

	     
