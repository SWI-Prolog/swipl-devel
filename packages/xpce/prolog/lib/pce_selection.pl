/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_selection, []).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file used to define `display   ->copy' and `display <-paste', which
are now part of the XPCE  kernel.   This  file  is retained for backward
compatibility reasons.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- format(user_error,
	  '[COMPATIBILITY: library(pce_selection) is obsolete]~n', []).
