/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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



