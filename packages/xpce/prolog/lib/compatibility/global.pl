/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module_transparent
	new_global/2.


new_global(Ref, Term) :-
	new(Ref, Term),
	send(Ref, lock_object, @on).

