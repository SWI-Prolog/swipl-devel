/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

portray(Obj) :-
	object(Obj),
	Obj = @(Ref), !,
	get(Obj, '_class_name', CN),
	format('@~w/~w', [Ref, CN]).
