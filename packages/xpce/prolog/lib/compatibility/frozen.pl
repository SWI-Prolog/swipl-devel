/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- pce_extend_class(device).

frozen(_Dev, _Val:bool) :->
	"Compatibility with version 4.1.x."::
	true.

:- pce_end_class.


