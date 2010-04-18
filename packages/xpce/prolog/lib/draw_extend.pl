/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2010, University of Amsterdam

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

:- module(draw_extend,
	  [ draw_begin_shape/3,
	    draw_end_shape/0
	  ]).
:- use_module(library(pce)).
:- require([ concat/3
	   , ensure_prefix/2
	   , forall/2
	   , member/2
	   ]).


:- pce_begin_class(draw_shape_class, class, "Handle class-level stuff").

variable(hidden_attributes, chain*, get, "Masked attributes").
variable(recognisers,	    chain*, get, "Event-handling recognisers").

initialise(Class, Name, Super) :->
	send(Class, send_super, initialise, Name, Super),
	(   get(Class, super_class, SuperClass),
	    send(SuperClass, instance_of, draw_shape_class)
	->  send(Class, slot, hidden_attributes,
		 SuperClass?hidden_attributes?copy),
	    send(Class, slot, recognisers,
		 SuperClass?recognisers)
	;   send(Class, slot, hidden_attributes, new(chain)),
	    send(Class, slot, recognisers, new(chain))
	).


hidden_attribute(Class, Attr:name) :->
	"Register a hidden attribute"::
	get(Class, hidden_attributes, Hidden),
	send(Hidden, add, Attr).


recogniser(Class, Recogniser:recogniser) :->
	"Register (prepend) a recogniser"::
	get(Class, recognisers, Recognisers),
	send(Recognisers, add, Recogniser).

:- pce_end_class.


draw_begin_shape(Name, Super, Summary, Recognisers) :-
	ensure_prefix(Name, PceName),
	ensure_prefix(Super, PceSuper),
	make_pce_super(PceSuper),
	pce_begin_class(PceName, PceSuper, Summary),
	forall(member(R, Recognisers),
	       send(@class, recogniser, R)).

make_pce_super(DrawClass) :-
	get(@pce, convert, DrawClass, class, _), !.
make_pce_super(DrawClass) :-
	concat(draw_, PceClass, DrawClass),
	get(@pce, convert, PceClass, class, _),
	new(NewClass, draw_shape_class(DrawClass, PceClass)).
