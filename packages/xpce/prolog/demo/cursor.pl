/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
