/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
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
