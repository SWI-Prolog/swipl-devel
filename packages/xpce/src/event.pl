/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

eventtest :-
	send(new(@p, picture), open),
	send(@p, recogniser, handler(any,
				     message(@pce, write_ln, @event?id))).
