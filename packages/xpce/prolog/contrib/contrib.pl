/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_contrib, [contribution/5]).

contribution('Rubiks Cube',			% Name
	     'The Rubiks Cube Game',		% Summary
	     'Christian Schlichtherle',		% Author
	     contrib('rubik/rubikpce'),		% Library to load
	     rubikpce).				% Toplevel goal
