/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_contrib, [contribution/5]).

contribution('Rubiks Cube',			% Name
	     'The Rubiks Cube Game',		% Summary
	     'Christian Schlichtherle',		% Author
	     contrib('rubik/rubikpce'),		% Library to load
	     rubikpce).				% Toplevel goal
