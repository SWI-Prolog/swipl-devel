/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_hyper, []).
:- use_module(library(pce)).

		 /*******************************
		 *     WHOLE-PART RELATION	*
		 *******************************/

:- pce_begin_class(partof_hyper, hyper,
		   "<-to is a part of <-from").

unlink_from(H) :->
	"->destroy the <-to part"::
	get(H, to, Part),
	(   object(Part),
	    send(Part, has_send_method, destroy)
	->  send(Part, destroy)
	;   free(Part)
	),
	free(H).

:- pce_end_class.
