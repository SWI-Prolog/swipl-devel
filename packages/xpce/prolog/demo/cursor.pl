/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_cursor,
	  [ cursor_demo/0
	  ]).
	  
:- use_module(library(pce)).

cursor_demo :-
	new(B, browser('Cursor Demo')),
	send(B, confirm_done, @off),
	send(B, select_message, message(B, cursor, @arg1?key)),
	send(@cursor_names, for_all, message(B, append, @arg1?name)),
	send(B, open).
