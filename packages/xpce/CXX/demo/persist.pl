/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- load_foreign_library(persist).

db :-
	new(@db, db(database, db)),
	get(@db, member, part1, @on, Part),
	send(@pce, format, 'Part %s\n', Part).
	
	
