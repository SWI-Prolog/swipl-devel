/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

which(Name, File) :-
	get(@pce, environment_variable, 'PATH', Path),
	new(F, file(Name)),
	send(F, find, Path, execute),
	get(F, absolute_path, File).
