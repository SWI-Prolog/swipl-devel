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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library defines some typical  dependency   relations  based on XPCE
hyper objects. Hyper objects relate two objects and are triggered by the
object-management layer if one of the connected object is destroyed.

The partof_hyper will destroy  the  related   `part'  if  the `whole' is
destroyed, while th mutual_dependency_hyper  destroys   the  other side,
regardless of which of the two is destroyed initially.

Example:

	new(_, partof_hyper(Frame, Dialog))

makes sure the Dialog is destroyed if the Frame disappears.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


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

:- pce_end_class(partof_hyper).


		 /*******************************
		 *  MUTUALLY DEPENDANT OBJECTS	*
		 *******************************/

:- pce_begin_class(mutual_dependency_hyper, hyper,
		   "<-to and <-from depend on each other").

unlink_from(H) :->
	"->destroy the <-to part"::
	get(H, to, Dependant),
	(   object(Dependant),
	    send(Dependant, has_send_method, destroy)
	->  send(Dependant, destroy)
	;   free(Dependant)
	),
	free(H).

unlink_to(H) :->
	"->destroy the <-from part"::
	get(H, from, Dependant),
	(   object(Dependant),
	    send(Dependant, has_send_method, destroy)
	->  send(Dependant, destroy)
	;   free(Dependant)
	),
	free(H).

:- pce_end_class(mutual_dependency_hyper).



